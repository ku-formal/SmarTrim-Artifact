open! Vocab

let unzip_src =
  {|from zipfile import ZipFile
from sys import argv
import os
dir = argv[1]
path = os.path.join(argv[1], argv[2])
with ZipFile(path, "r") as zip_ref:
  zip_ref.extract("solc.exe", path=dir)
os.remove(path)
os.rename(os.path.join(dir, "solc.exe"), path)
|}
;;

let lwt_unzip dirpath version =
  Os.lwt_system_call_assert ~errmsg:"failed to unzip"
    ("python3", [| "python3"; "-c"; unzip_src; dirpath; version |])
;;

let json_url = {%string|https://binaries.soliditylang.org/%{Os.get_platform_name ()}/list.json|}

let is_solc_installed (v : Ver.t) : bool =
  let vstring = Ver.to_string v in
  let expected_filepath = Os.cat_fromroot [ ".solc"; vstring ] in
  if Sys.file_exists expected_filepath && Sys.is_regular_file expected_filepath then true
    (* todo: check commands exists well *)
  else false
;;

let solc_download (v : Ver.t) : unit Lwt.t =
  let module J = Yojson.Basic.Util in
  let open Lwt.Syntax in
  let member mem json : Yojson.Basic.t Lwt.t =
    let v = J.member mem json in
    match v with
    | `Null -> Pp.invalid_argf "Solc.Install : json member not found : %s" mem
    | _ -> Lwt.return v
  in
  let* json_content = Os.Pybind.lwt_request_bytes json_url in
  let* json =
    try Lwt.return @@ Yojson.Basic.from_string json_content
    with Yojson.Json_error _errmsg -> Pp.invalid_argf "Solc.Install : json parsing failed"
  in
  let* json_releases = member "releases" json in
  let* json_builds =
    match J.member "builds" json with
    | `List l -> Lwt.return l
    | _ -> Pp.invalid_argf "Solc.Install : The server gave us an unexpected json format"
  in
  let version_string = Ver.to_string v in
  let meta =
    List.find_opt
      (fun j ->
        let version' = J.member "version" j in
        J.to_string_option version' = Some version_string)
      json_builds
  in
  let* meta =
    match meta with
    | Some meta -> Lwt.return meta
    | None ->
      Pp.invalid_argf
        "Solc.Install : maybe unsupported version by binaries.solidity.org. Version: %a"
        Ver.pp_human v
  in
  let* sha256 = member "sha256" meta in
  let* sha256_expect =
    match sha256 with
    | `String s -> Lwt.return s
    | _ -> Pp.failwithf "Solc.Install : The server gave us an unexpected json format"
  in
  let* version_addr =
    match J.member version_string json_releases with
    | `String s -> Lwt.return s
    | _ ->
      Pp.failwithf "Solc.Install : maybe unsupported version by binaries.solidity.org. Version: %a"
        Ver.pp_human v
  in
  let proc_url =
    {%string|https://binaries.soliditylang.org/%{Os.get_platform_name ()}/%{version_addr}|}
  in
  let* byte = Os.Pybind.lwt_request_bytes proc_url in
  let hash = Digestif.SHA256.digest_string byte in
  let hash = Pp.str Digestif.SHA256.pp hash in
  let hash = "0x" ^ hash in
  let* () =
    if hash = sha256_expect then Lwt.return ()
    else
      Pp.invalid_argf "Solc.Install : Checksums do not match (expected: %s; actual: %s)"
        sha256_expect byte
  in
  let filename = Os.cat_fromroot [ ".solc"; version_string ] in
  Out_channel.with_open_bin filename (fun oc -> Out_channel.output_string oc byte);
  let* () =
    (* if the file is zipped, unzip *)
    if String.ends_with ~suffix:".zip" proc_url then
      lwt_unzip (Os.cat_fromroot [ ".solc" ]) version_string
    else Lwt.return ()
  in
  Unix.chmod filename 0o775;
  Lwt.return ()
;;

let install_solc_unit ?(forced = false) (ver : Ver.t) : unit Lwt.t =
  if (not forced) && is_solc_installed ver then
    Pp.failwithf "Solc already installed : %a" Ver.pp_human ver
  else solc_download ver
;;

let install_solc ?(forced = false) (cmd : string) : unit =
  match cmd with
  | _ when Option.is_some (Ver.of_string cmd) ->
    Lwt_main.run @@ install_solc_unit ~forced (Ver.of_string_exn cmd)
  | "all" ->
    let open Lwt.Syntax in
    let vers = Ver.versions in
    let install_solc_unit cmd =
      try install_solc_unit ~forced cmd with
      | Failure s ->
        let* () = Lwt_io.printf "A thread raised an error: Failure(%s)\n" s in
        let* () = Lwt_io.(flush stdout) in
        Lwt.return ()
      | Invalid_argument s ->
        let* () = Lwt_io.printf "A thread raised an error: Invalid_argument(%s)\n" s in
        let* () = Lwt_io.(flush stdout) in
        Lwt.return ()
      | _ -> Lwt.return ()
    in
    let results = Lwt_list.map_p install_solc_unit vers in
    ignore @@ Lwt_main.run results
  | _ -> Pp.invalid_argf "Unsupported cmd : %s" cmd
;;
