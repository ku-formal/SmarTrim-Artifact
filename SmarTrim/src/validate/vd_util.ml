(** Validator utilities. *)

let devnull = Unix.openfile Filename.null [ Unix.O_RDWR ] 0o666

let get_datetime_string () =
  let time = Unix.time () in
  let time = Unix.localtime time in
  Printf.sprintf "%02d%02d_%02d%02d%02d" (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min
    time.tm_sec
;;

(** top workspace directory *)
let workspace = Os.cat_fromroot [ "_validator_workspace" ]

type config = {
  dir : string;  (** directory, [project_dir] is a subdirectory of this *)
  project_dir : string;  (** foundry project directory *)
  oc : out_channel;  (** out channel for logging *)
}

(** initialize work directories *)
let init_space () : config =
  if not @@ Sys.file_exists workspace then Sys.mkdir workspace 0o755;
  let state = Random.State.make_self_init () in
  let workdir = get_datetime_string () ^ "_" ^ string_of_int @@ Random.State.bits state in
  let dir = Os.cat [ workspace; workdir ] in
  Sys.mkdir dir 0o755;
  let foundry_dir = Os.cat [ dir; "project" ] in
  Sys.mkdir foundry_dir 0o755;
  let log = Os.cat [ dir; ".log" ] in
  { dir; project_dir = foundry_dir; oc = open_out log }
;;

(** Usage: [script_content version]. sourcecode of script contract. *)
let script_content version =
  {%string|// SPDX-License-Identifier: MIT
pragma solidity ^%{version};

contract MigrationScript {
  address public owner;
  uint public last_completed_migration;

  event MigrationCompleted(uint migration_number);

  constructor() public {
    owner = msg.sender;
  }

  modifier restricted() {
    require(msg.sender == owner, "This function is restricted to the contract's owner");
    _;
  }

  function setCompleted(uint completed) public restricted {
    last_completed_migration = completed;
    emit MigrationCompleted(completed);
  }

  function upgrade(address new_address) public restricted {
    MigrationScript upgraded = MigrationScript(new_address);
    upgraded.setCompleted(last_completed_migration);
  }
}|}
;;

let solc_last_minor (version : string) =
  match version with
  | _ when String.starts_with "0.4" version -> "0.4.26"
  | _ -> "0.4.26" (* no reason for default, just since it fits well for many benchmarks *)
;;

let toml_content solc_version initial_balance =
  {%string|[profile.default]
src = "src"
out = "out"
libs = ["lib"]

# Compiler settings
solc_version = "%{solc_last_minor solc_version}"
evm_version = "cancun"
optimizer = true
optimizer_runs = 200
initial_balance = "%{initial_balance}"

# Gas settings
gas_price = 0
gas_limit = 10000000  

# Network settings
[rpc_endpoints]
development = "http://127.0.0.1:8545"

# Test settings
[fuzz]
runs = 1000

[invariant]
runs = 1000
depth = 100

# Deployment settings
[profile.default.networks.development]
url = "http://127.0.0.1:8545"
|}
;;

(** Event name of revert = ["valismartest_falsified"]. That is, contracts that violates our safety
    condition will be validated with [require(safety_cond, %{revert_name});] (if delayed safety
    checking is not performed). *)
let revert_name = "valismartest_falsified"

module Vd_error = struct
  module Internal = struct
    type t =
      | JSON_LOAD_FAILED
      | INIT_FAILED
      | SUBPROC_SIGNALED  (** the subprocess is either stopped or killed by os signal *)
    [@@deriving show { with_path = false }]
  end

  type t = COMPILE_ERR | TEST_FAILED | INTERNAL_ERR of Internal.t
  [@@deriving show { with_path = false }]

  let to_int = function
    | COMPILE_ERR -> 1
    | TEST_FAILED -> 2
    | INTERNAL_ERR i -> (
      match i with JSON_LOAD_FAILED -> 100 | INIT_FAILED -> 101 | SUBPROC_SIGNALED -> 102)
  ;;

  let result_to_int (v : ('ok, t) result) = match v with Ok _ -> 0 | Error t -> to_int t
end

(***************************)
(* Unix utilities          *)
(***************************)

let open_subproc_and_wait2 prog args ~when_error : (string * string, Vd_error.t) result =
  let f =
    let open Lwt.Syntax in
    let command = (prog, args) in
    let process = Lwt_process.open_process_full command in
    let read_stdout = Lwt_io.read process#stdout in
    let read_stderr = Lwt_io.read process#stderr in
    let* stdout, stderr = Lwt.both read_stdout read_stderr in
    let* status = process#status in
    Lwt.return (stdout, stderr, status)
  in
  let out, err, state = Lwt_main.run f in
  match state with
  | WEXITED 0 -> Ok (out, err)
  | WEXITED _ -> Error (when_error out err)
  | _ -> Error (INTERNAL_ERR SUBPROC_SIGNALED)
;;
