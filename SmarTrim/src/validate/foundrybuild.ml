open Concrete
open Vd_util

open struct
  module O = Out_channel
  module P = Printf

  let ( let* ) = Result.bind

  let write_codes (foundry_dir : string) solidity_content (tseq : Tseq.t) =
    (* add Counter file to the project *)
    O.with_open_text
      (Os.cat [ foundry_dir; "src"; "Counter.sol" ])
      (fun oc -> P.fprintf oc "%s\n" solidity_content);
    O.with_open_text
      (Os.cat [ foundry_dir; ".."; "Counter.sol" ])
      (fun oc -> P.fprintf oc "%s\n" solidity_content (* for memo purpose *));
    (* write migration script *)
    O.with_open_text
      (Os.cat [ foundry_dir; "script"; "Counter.s.sol" ])
      (fun oc -> P.fprintf oc "%s\n" @@ script_content tseq.compiler_version);
    (* write toml setting *)
    O.with_open_text
      (Os.cat [ foundry_dir; "foundry.toml" ])
      (fun oc ->
        P.fprintf oc "%s\n" @@ toml_content tseq.compiler_version (Value.show_solexp tseq.init_eth));
    (* write test script (Counter.t.sol) *)
    let testfile = Testwriter.get_testfile tseq in
    let testfile = testfile ^ "\n\n" ^ Testwriter.get_attacker tseq in
    O.with_open_text
      (Os.cat [ foundry_dir; "test"; "Counter.t.sol" ])
      (fun oc -> P.fprintf oc "%s\n" testfile);
    O.with_open_text
      (Os.cat [ foundry_dir; ".."; "Counter.t.sol" ])
      (fun oc -> P.fprintf oc "%s\n" testfile);
    (* write for recording *)
    let tseq = Tseq.to_compact tseq in
    let json_content = Reports.Exploit_r.Tseq.yojson_of_t tseq |> Yojson.Safe.pretty_to_string in
    O.with_open_text
      (Os.cat [ foundry_dir; ".."; "tseq.json" ])
      (fun oc -> P.fprintf oc "%s\n" json_content)
  ;;
end

let run config (solidity_content : string) (tseq : Tseq.t) : (unit, Vd_error.t) result =
  (* init forge *)
  let* _s_out, _s_err =
    open_subproc_and_wait2 "forge" [| "forge"; "init"; "--no-git"; "--force"; config.project_dir |]
      ~when_error:(fun _ s_err ->
        P.fprintf config.oc "%s\n%!" s_err;
        INTERNAL_ERR INIT_FAILED)
  in
  write_codes config.project_dir solidity_content tseq;
  (* compile *)
  let solc_v = Solc.Ver.of_string_exn tseq.compiler_version in
  let solc_v = if solc_v < Solc.version 0 5 0 then Solc.version 0 4 26 else solc_v in
  let solc_path = Solc.get_solc_path solc_v in
  let* s_out, _s_err =
    open_subproc_and_wait2 "forge"
      [| "forge"; "build"; "-vvvvv"; "--root"; config.project_dir; "--use"; solc_path |]
      ~when_error:(fun s_out s_err ->
        P.fprintf config.oc "%s\n%!" s_out;
        P.fprintf config.oc "%s\n%!" s_err;
        COMPILE_ERR)
  in
  P.fprintf config.oc "%s\n%!" s_out;
  (* test *)
  let* s_out, _s_err =
    open_subproc_and_wait2 "forge"
      [| "forge"; "test"; "-vvvvv"; "--root"; config.project_dir; "--use"; solc_path |]
      ~when_error:(fun s_out _ ->
        P.fprintf config.oc "%s\n%!" s_out;
        TEST_FAILED)
  in
  P.fprintf config.oc "%s\n%!" s_out;
  Ok ()
;;
