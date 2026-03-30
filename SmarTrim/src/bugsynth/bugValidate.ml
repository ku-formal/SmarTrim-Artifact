open Options
open Semantics
open Vocab
open BugWorkspace

let kill_verify_timeout () = !verify_timeout + 300
let kill_exploit_timeout () = !exploit_timeout + 300

let cmd_exploit inputfile main_name (solv : Solc.Ver.t) : string =
  let main_name = if BatString.starts_with main_name "$" then "\\" ^ main_name else main_name in
  ["timeout"; string_of_int (kill_exploit_timeout());
   "./main.native"; "-input"; inputfile; "-main"; if main_name = "" then "\"\"" else main_name;
   "-report"; "-verbose";
   "-solv"; Solc.Ver.to_string solv;
   "-mode"; "exploit"; "-refined_vcgen"; (* "-uninterp_nonlinear"; *)
   "io"; "kill"; "leak"; "re"; (* "tx"; "assert"; *)
   "-init_eth"; "10";
   "-exploit_timeout"; string_of_int !exploit_timeout;
   "-z3timeout"; string_of_int !z3timeout;
   "-exploit_vrep"; verify_report_name inputfile]
  @ (if !E.skip_external_call then ["-exploit_skip_external_call"] else [])
  @ (if !B.no_chase then ["-exploit_rand"] else ["-exploit_chase"])
  @ ["-outdir"; exploit_report_dir ()]
  @ [">"; exploit_log_name inputfile; "2>&1"] (* https://linuxize.com/post/bash-redirect-stderr-stdout/ *)
  |> string_of_list ~first:"" ~last:"" ~sep:" " Vocab.id

let cmd_verify inputfile main_name (solv : Solc.Ver.t) : string =
  let main_name = if BatString.starts_with main_name "$" then "\\" ^ main_name else main_name in
  ["timeout"; string_of_int (kill_verify_timeout());
   "./main.native"; "-input"; inputfile; "-main"; if main_name = "" then "\"\"" else main_name;
   "-report"; "-verbose";
   "-solv"; Solc.Ver.to_string solv;
   "-mode"; "verify"; "-refined_vcgen"; "-uninterp_nonlinear"; "-alarm_filter";
   "io"; "kill"; "leak"; "re"; (* "tx"; "assert"; *)
   "-verify_timeout"; string_of_int !verify_timeout;
   "-z3timeout"; string_of_int !z3timeout;
   "-outdir"; verify_report_dir ()]
  @ [">"; verify_log_name inputfile; "2>&1"] (* https://linuxize.com/post/bash-redirect-stderr-stdout/ *)
  |> string_of_list ~first:"" ~last:"" ~sep:" " Vocab.id
  (* assert (Sys.command cmd = 0) *)

type unproven = Query.ID.t
type disproven = Query.ID.t
type elapsed = float
type cmd = string
type err = bool

let mk_err_report timeout ~filename ~errmsg ~time ~reportdir =
  let _ = errmsg in
  let reportfile = Filename.concat reportdir (BatString.rchop ~n:4 (Filename.basename filename) ^ ".json") in
  if not timeout then
    (if not (Sys.file_exists reportfile) then
      Json.mk_err_report ~filename ~errmsg:"Not timeout" ~time ~reportdir ()) (* TODO: killed ? *)
  else
    let _ = assert (not (Sys.file_exists reportfile)) in
    Json.mk_err_report ~filename ~errmsg:"Timeout" ~time ~reportdir ()

let do_verify inputfile : unproven BatSet.t * elapsed * cmd * err =
  try
    let (main_name,solv) = get_main_solv inputfile in
    let cmd = cmd_verify inputfile main_name solv in
    let before = Unix.gettimeofday () in
    let _ = Sys.command cmd in
    let elapsed = Unix.gettimeofday () -. before in
    try
      let unproven = Verify.Report.get_qs_from_json_report 
        ~status:"unproven" (verify_report_name inputfile) 
      in
      (BatSet.of_list unproven, elapsed, cmd, false)
    with _ -> (* e.g., compile error *)
      (BatSet.singleton { Query.ID.kind = IO; loc = Query.line2loc (-1); exp = "V_DUMMY" }, elapsed, cmd, true)
  with _ -> (* e.g., vyper contract *)
    (BatSet.singleton { Query.ID.kind = IO; loc = Query.line2loc (-1); exp = "V_DUMMY" }, 0.0, "unknown cmd", true)

let do_exploit inputfile : disproven BatSet.t * elapsed * cmd * err =
  try
    let (main_name,solv) = get_main_solv inputfile in
    let cmd = cmd_exploit inputfile main_name solv in
    let before = Unix.gettimeofday () in
    let _ = Sys.command cmd in
    let elapsed = Unix.gettimeofday () -. before in
    try
      let disproven = Exploit.E_report.disproven_lst_from_json_report (exploit_report_name inputfile) in
      let err =
        let j = Yojson.Basic.from_file (exploit_report_name inputfile) in
        match Yojson.Basic.Util.member "errMsg" j with
        | `Null -> false
        | _ -> true in
      (BatSet.of_list disproven, elapsed, cmd, err)
    with _ -> (* e.g., compile error *)
      (BatSet.singleton { Query.ID.kind = IO; loc = Query.line2loc (-1); exp = "V_DUMMY" }, elapsed, cmd, true)
  with _ -> (* e.g., vyper contract *) (* TODO: remove *)
    (BatSet.singleton { Query.ID.kind = IO; loc = Query.line2loc (-1); exp = "V_DUMMY" }, 0.0, "vyper contract", true)

let do_validate inputfile : Mutant.t =
  let is_buggy unproven disproven =
    BatSet.cardinal unproven > 0
    && BatSet.equal unproven disproven
  in
  let is_pending unproven disproven =
    BatSet.cardinal unproven > 0
    && not (BatSet.equal unproven disproven)
    && BatSet.subset disproven unproven
  in
  let is_safe unproven = (BatSet.cardinal unproven = 0)
  in
  let (unproven,time_v,cmd_v,err_v) = do_verify inputfile in
  let (disproven,time_e,cmd_e,err_e) =
    if not err_v then do_exploit inputfile
    else
      let _ = Json.mk_err_report ~filename:inputfile ~errmsg:"Verify error" ~time:0.0 ~reportdir:(exploit_report_dir()) () in
      (BatSet.singleton { Query.ID.kind = IO; loc = Query.line2loc (-1); exp = "V_DUMMY" }, 0.0, "skip", false)
  in
  let timeout_v = time_v > float_of_int(kill_verify_timeout()) in
  let timeout_e = time_e > float_of_int(kill_exploit_timeout()) in
  let _ = mk_err_report timeout_v ~filename:inputfile ~errmsg:"Timeout" ~time:time_v ~reportdir:(verify_report_dir()) in
  let _ = mk_err_report timeout_e ~filename:inputfile ~errmsg:"Timeout" ~time:time_e ~reportdir:(exploit_report_dir()) in
  { path = inputfile;
    cmd_v = cmd_v;
    cmd_e = cmd_e;
    time_v = time_v;
    time_e = time_e;
    buggy   = is_buggy unproven disproven;
    safe    = is_safe unproven;
    pending = is_pending unproven disproven && not err_v && not err_e;
    err_v = not timeout_v && err_v;
    err_e = not timeout_e && err_e;
    timeout_v = timeout_v;
    timeout_e = timeout_e;
  }
