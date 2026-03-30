open Verify
open Vocab

(* root always indicates *)
let get_cands_dir root = Filename.concat root "candidates"
let get_knowledge root = Filename.concat root "knowledge.json"

let get_report_dir root = Verifier.get_report_dir root
let get_log_dir root = Filename.concat root "log"

(* XXX : necessary? already in verismart.json *)
let get_vr root = Verifier.get_vr root
let get_rr root = Verifier.get_rr root
let get_jr root = Verifier.get_jr root


(***********************)
(*** Make workspace  ***)
(***********************)

let mk_mmddhhmm () =
  let tm = Unix.localtime (Unix.time()) in
  let mm = zfill 2 (string_of_int (tm.tm_mon+1)) in
  let dd = zfill 2 (string_of_int tm.tm_mday) in
  let hh = zfill 2 (string_of_int tm.tm_hour) in
  let mm' = zfill 2 (string_of_int tm.tm_min) in
  mm ^ dd ^ "_" ^ hh ^ mm'

let setup () =
  let _ = if !Options.outdir = "" then 
    Options.outdir := Sys.getcwd () ^ "/repair_results/" ^ mk_mmddhhmm () 
  in
  let root = !Options.outdir in
  List.iter (fun d -> assert (Sys.command ("mkdir " ^ d) = 0))
  [root; get_report_dir root; get_cands_dir root; get_log_dir root]

(* Does not make patch dir *)
let make_sub outroot =
  List.iter (fun d -> assert (Sys.command ("mkdir " ^ d) = 0))
  [outroot; get_report_dir outroot; get_log_dir outroot]

(****************************)
(*** Read from workspace  ***)
(****************************)

let get_vul_alarms file = file |> Filename.dirname |> get_vr |> FixTarget.get_alarms

let get_reg_alarms file = file |> Filename.dirname |> get_rr |> FixTarget.get_regressions

let read_json file =
  let json = Yojson.Basic.from_file file in
  match Yojson.Basic.Util.member "inv_size" json with
  | `Int n -> n
  | _ -> assert false

let get_inv_size_cand cand_file = read_json (Filename.dirname cand_file ^ "/reports/verismart.json")
