open BugWorkspace

open struct
  module J = Yojson.Basic.Util
end

let count_bug_typs mut_buggy =
  let fids = List.map (fun f -> snd (BatString.replace ~str:(Filename.basename f) ~sub:".sol" ~by:"")) mut_buggy in
  List.fold_left (fun (acc_io,acc_el,acc_su,acc_re,acc_tx,acc_av) fid ->
    let json = Yojson.Basic.from_file (exploit_report_dir () ^ "/" ^ fid ^ ".json") in
    let io = json |> J.member "summary" |> J.member "IO" |> J.to_int in
    let el = json |> J.member "summary" |> J.member "ETH_LEAK" |> J.to_int in
    let su = json |> J.member "summary" |> J.member "KA" |> J.to_int in
    let re = json |> J.member "summary" |> J.member "RE_EL" |> J.to_int in
    let tx = 0 in (* json |> J.member "summary" |> J.member "TX_ORG" |> J.to_int in *) (* XXX *)
    let av = json |> J.member "summary" |> J.member "ASSERT" |> J.to_int in
    (acc_io + io, acc_el + el, acc_su + su, acc_re + re, acc_tx + tx, acc_av + av)
  ) (0,0,0,0,0,0) fids

let get_err_msg_from_report report =
  report |> Yojson.Basic.from_file |> J.member "errMsg"
  |> (fun msg -> match msg with `Null -> "" | `String m -> m | _ -> assert false)

let mk_seed_summary' seed =
  let path = Seed.get_path seed in
  let fid = snd (BatString.replace ~str:(Filename.basename path) ~sub:".sol" ~by:"") in
  let report = verify_report_name path in
  let cmd = Seed.get_cmd seed in
  [fid; get_err_msg_from_report report; cmd]

let mk_seed_summary seeds outname =
  let header = ["fid"; "err"; "cmd"] in
  seeds
  |> List.map mk_seed_summary'
  |> (fun rows -> header :: rows)
  |> Csv.save (Filename.concat !Options.outdir outname)

let mk_mutant_summary' mutant =
  let path = Mutant.get_path mutant in
  let fid = snd (BatString.replace ~str:(Filename.basename path) ~sub:".sol" ~by:"") in
  let report_v, report_e = verify_report_name path, exploit_report_name path in
  let err_v = get_err_msg_from_report report_v in
  let err_e = get_err_msg_from_report report_e in
  [fid; err_v; err_e]

let mk_mutant_summary mutants outname =
  let header = ["fid"; "err_v"; "err_e"] in
  mutants
  |> List.map mk_mutant_summary'
  |> (fun rows -> header :: rows)
  |> Csv.save (Filename.concat !Options.outdir outname)

let mk_mutant_cmd mutants outname =
  let cmd_of_mutant m = Mutant.get_cmd_v m ^ "\n" ^ Mutant.get_cmd_e m in
  let str = Vocab.string_of_list ~first:"" ~sep:"\n\n" ~last:"\n" cmd_of_mutant mutants in
  let fp = open_out (Filename.concat !Options.outdir outname) in
  Printf.fprintf fp "%s" str;
  close_out fp

let mk_ground_truth_row fid res =
  let get_lines res kind =
    res
    |> List.filter (fun j -> j |> J.member "kind" |> J.to_string = kind)
    |> List.map (fun j -> j |> J.member "line" |> J.to_int)
    |> List.sort Stdlib.compare
    |> Vocab.string_of_list ~first:"" ~last:"" ~sep:"/" string_of_int in
  let get_fnames res kind =
    res
    |> List.filter (fun j -> j |> J.member "kind" |> J.to_string = kind)
    |> List.map (fun j -> j |> J.member "lastTxName" |> J.to_string)
    |> BatList.unique
    |> Vocab.string_of_list ~first:"" ~last:"" ~sep:"/" Vocab.id in
  let is_func_gt_complete res =
    res
    |> List.for_all (fun j -> j |> J.member "isUniqueTxToLine" |> J.to_bool)
    |> string_of_bool in
  [fid;
   get_lines res "IO";       get_fnames res "IO";
   get_lines res "ETH_LEAK"; get_fnames res "ETH_LEAK";
   get_lines res "KA";       get_fnames res "KA";
   get_lines res "RE_EL";    get_fnames res "RE_EL";
   get_lines res "ASSERT";   get_fnames res "ASSERT";
   is_func_gt_complete res]

let mk_ground_truth mutants' =
  let header = ["fid"; "io"; "io_f"; "el"; "el_f"; "su"; "su_f"; "re"; "re_f"; "assert"; "assert_f"; "complete_func_gt"] in
  mutants'
  |> List.map exploit_report_name
  |> List.map (fun report ->
       let fid = snd (BatString.replace ~str:(Filename.basename report) ~sub:".json" ~by:"") in
       let json = Yojson.Basic.from_file report in
       let queries = json |> J.member "summary" |> J.member "queries" |> J.to_int in
       let disproven = json |> J.member "summary" |> J.member "disproven" |> J.to_int in
       let _ = assert (queries = disproven) in
       let res = json |> J.member "result" |> J.to_list in
       mk_ground_truth_row fid res)
  |> (fun rows -> [header] @ rows)
  |> (fun csv -> Csv.save (Filename.concat !Options.outdir "ground_truth.csv") csv)

let mk_txt_log ~filepath ~str =
  let fp = open_out filepath in
  Printf.fprintf fp "%s" str;
  close_out fp

let mk_buggy_mutant_meta mutants =
  let metafile = meta_path () in
  let rows = Csv.Rows.load ~has_header:true ~header:meta_header metafile in
  List.map (fun mutant ->
    let org_fid = get_org_fid mutant in
    let fid = snd (BatString.replace ~str:(Filename.basename mutant) ~sub:".sol" ~by:"") in
    let row = List.find (fun r -> org_fid = Csv.Row.find r "id") rows in
    let lst = Csv.Row.to_list row in
    let lst = BatList.modify_at 0 (fun _ -> fid) lst in
    let lst = BatList.modify_at 4 (fun _ -> string_of_int (BatFile.count_lines mutant)) lst in
    lst
  ) mutants
  |> (fun res -> [meta_header] @ res)
  |> (fun csv -> Csv.save (Filename.concat !Options.outdir "bug_meta.csv") csv)


let print : BugTemplate.at list * Seed.t list * Mutant.t list -> unit
= fun (templates, seeds, mutants) ->
  let seed_safe = List.filter Seed.is_safe seeds in

  let mut_buggy = List.filter Mutant.is_buggy mutants in
  let mut_safe = List.filter Mutant.is_safe mutants in
  let mut_pending = List.filter Mutant.is_pending mutants in
  let mut_timeout = List.filter Mutant.is_timeout mutants in
  let mut_err = List.filter Mutant.is_err mutants in

  let (io,el,su,re,tx,av) = count_bug_typs (List.map Mutant.get_path mut_buggy) in
  print_endline "\n========== Statistics ==========";
  print_endline ("# Template         : " ^ string_of_int (List.length templates));
  print_endline ("# Seed             : " ^ string_of_int (List.length seeds));
  print_endline ("# Bug-free Seed    : " ^ string_of_int (List.length seed_safe));

  print_endline "";
  let srate = (float_of_int (List.length mut_buggy) /. float_of_int (List.length mutants)) *. 100. in
  print_endline ("# Mutant           : " ^ string_of_int (List.length mutants));
  print_endline ("- Buggy            : " ^ string_of_int (List.length mut_buggy) ^ " (" ^ string_of_float srate ^ "%)");
  print_endline ("- Safe             : " ^ string_of_int (List.length mut_safe));
  print_endline ("- Unclassified     : " ^ string_of_int (List.length mut_pending));
  print_endline ("- Timeout          : " ^ string_of_int (List.length mut_timeout));
  print_endline ("- Error            : " ^ string_of_int (List.length mut_err));

  print_endline "";
  print_endline ("# Injected Bugs    : " ^ string_of_int (io + el + su + re + tx + av));
  print_endline ("- IO               : " ^ string_of_int io);
  print_endline ("- ETH_LEAK         : " ^ string_of_int el);
  print_endline ("- KA               : " ^ string_of_int su);
  print_endline ("- RE_EL            : " ^ string_of_int re);
  print_endline ("- TX_ORG           : " ^ string_of_int tx);
  print_endline ("- ASSERT           : " ^ string_of_int av);

  let seed_err_v, seed_timeout_v = List.filter Seed.is_err seeds, List.filter Seed.is_timeout seeds in
  let mut_err_v, mut_timeout_v = List.filter Mutant.is_err_v mutants, List.filter Mutant.is_timeout_v mutants in
  let mut_err_e, mut_timeout_e = List.filter Mutant.is_err_e mutants, List.filter Mutant.is_timeout_e mutants in

  print_endline "";
  print_endline "==== Validation Exceptions ====";
  print_endline ("# Verifier (seed)      : " ^ string_of_int (List.length seed_err_v + List.length seed_timeout_v));
  print_endline ("- Error                : " ^ string_of_int (List.length seed_err_v));
  print_endline ("- Timeout              : " ^ string_of_int (List.length seed_timeout_v));
  print_endline ("# Verifier (mutant)    : " ^ string_of_int (List.length mut_err_v + List.length mut_timeout_v));
  print_endline ("- Error                : " ^ string_of_int (List.length mut_err_v));
  print_endline ("- Timeout              : " ^ string_of_int (List.length mut_timeout_v));
  print_endline ("# Disprover (mutant)   : " ^ string_of_int (List.length mut_err_e + List.length mut_timeout_e));
  print_endline ("- Error                : " ^ string_of_int (List.length mut_err_e));
  print_endline ("- Timeout              : " ^ string_of_int (List.length mut_timeout_e));

  mk_txt_log
    ~filepath:(Filename.concat !Options.outdir "bugfree_seed.txt")
    ~str:(Vocab.string_of_list ~first:"" ~sep:"\n" ~last:"\n" Vocab.id (List.map Seed.get_path seed_safe));

  if not (Sys.file_exists (Filename.concat !Options.input "bugfree_seed.txt"))
    then mk_seed_summary seeds "summary_seed.csv"; (* make summary when bugfree seeds were newly identified *)

  mk_txt_log
    ~filepath:(Filename.concat !Options.outdir "safe_mutant.txt")
    ~str:(Vocab.string_of_list ~first:"" ~sep:"\n" ~last:"\n" Vocab.id (List.map Mutant.get_path mut_safe));

  mk_txt_log
    ~filepath:(Filename.concat !Options.outdir "unclassified_mutant.txt")
    ~str:(Vocab.string_of_list ~first:"" ~sep:"\n" ~last:"\n" Vocab.id (List.map Mutant.get_path mut_pending));

  mk_ground_truth (List.map Mutant.get_path mut_buggy);
  mk_buggy_mutant_meta (List.map Mutant.get_path mut_buggy);

  mk_mutant_summary mutants "summary_mutant.csv";
  mk_mutant_cmd mutants "cmd_mutant.txt";
