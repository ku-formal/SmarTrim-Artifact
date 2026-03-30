open! Frontend
open Vocab
open BugWorkspace
open BugValidate
open BugTemplate
open Seed

let debug_ats ats =
  List.iteri (fun i at ->
    print_endline ("* Abstract Template " ^ string_of_int (i+1));
    print_endline (to_string_at at ^ "\n")
  ) ats

let debug_cts f cts =
  print_endline ("===== " ^ f ^ " =====");
  List.iteri (fun i ct ->
    print_endline ("* Concrete Template " ^ string_of_int (i+1));
    print_endline (to_string_ct ct ^ "\n")
  ) cts

(* TODO: separate main, solv ? *)
let get_main_solv ~is_seed file lines : (string * Solc.Ver.t option) =
  if is_seed then 
    let main, solv = BugWorkspace.get_main_solv file in
    main, Some solv
  else if BatString.starts_with (List.hd lines) "pragma " then
    List.hd lines
    |> BatString.lchop ~n:(String.length "pragma solidity ")
    |> BatString.rchop ~n:1
    |> (fun solv -> ("", Some (Solc.Ver.of_string_exn solv)))
  else ("", None)

let preprocess ~is_seed file =
  let lines = BatList.of_enum (BatFile.lines_of file) in
  let (main_name, solv) = get_main_solv ~is_seed file lines in
  Options.solc_ver := solv;
  let solv, json_ast = Solc.get_json_ast ~silent:true solv file in
  Options.solc_ver := Some solv;
  let pgm0 = Translator.run json_ast lines in
  Options.main_contract := if main_name = "" then (BatList.last pgm0).name else main_name;
  let pgm = Preprocess.run pgm0 in
  let pgm = Preprocess2.run pgm in
  let pgm = MakeCfg.run pgm in
  let global = Global.make_global_info pgm lines in
  (pgm0 |> Preprocess.copy |> Preprocess.rename, pgm, global, lines)

let extract_from_seeds : string list -> at list
= fun seeds ->
  List.fold_left (fun acc f ->
    let (pgm,_,global,_) = preprocess ~is_seed:false f in
    let tid = snd (BatString.replace ~str:(Filename.basename f) ~sub:".sol" ~by:"") in
    let at = BugExtract.extract global tid pgm in
    if List.exists (fun at' -> BugTemplate.eq_at at at') acc then acc
    else acc @ [at]
  ) [] seeds

let collect_templates dir : BugTemplate.at list =
  print_endline ("[INFO] Collecting Templates ...");
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun s -> BatString.ends_with s ".sol")
  |> List.map (fun s -> Filename.concat dir s)
  |> extract_from_seeds

let mk_abi (mut_path, solv, main) =
  let py = Py.import "src.bugsynth.py.gen_abi_bin" in
  let solv = Solc.Ver.to_string solv in
  try
    let (mut_path, solv, main) = (Py.String.of_string mut_path, Py.String.of_string solv, Py.String.of_string main) in
    let abi = Py.Module.get_function py "gen_abi" [|mut_path; solv; main|] in
    (true, Py.String.to_string abi)
  with _ -> (false,"")

let mk_bin ~is_runtime_bc (mut_path, solv, main) =
  let py = Py.import "src.bugsynth.py.gen_abi_bin" in
  let solv = Solc.Ver.to_string solv in
  try
    let (mut_path, solv, main, is_runtime_bc) =
      (Py.String.of_string mut_path, Py.String.of_string solv, Py.String.of_string main, Py.Bool.of_bool is_runtime_bc) in
    let bin = Py.Module.get_function py "gen_bc" [|mut_path; solv; main; is_runtime_bc|] in
    (true, Py.String.to_string bin)
  with _ -> (false, "")

(* generate abi/bin files too *)
let apply_wrapper main solv seed ct lines =
   let seed_id = snd (BatString.replace ~str:(Filename.basename (Seed.get_path seed)) ~sub:".sol" ~by:"") in
   let (success, lines') = BugTemplate.apply solv seed_id ct lines in
   if not success then print_endline ("[WARNING] template application failed - " ^ seed_id)
   else

   let mut_path = Filename.concat (mutant_dir()) (fst ct ^ "." ^ seed_id ^ ".sol") in
   let _ = assert (not (Sys.file_exists mut_path)) in
   let sol_str = string_of_list ~first:"" ~last:"\n" ~sep:"\n" Vocab.id lines' in
   let fp_sol = open_out mut_path in
   Printf.fprintf fp_sol "%s" sol_str;
   close_out fp_sol;

   let (b1, abi_str) = mk_abi (mut_path, solv, main) in
   let (b2, bin_str) = mk_bin ~is_runtime_bc:false (mut_path, solv, main) in
   let (b3, bin_runtime_str) = mk_bin ~is_runtime_bc:true (mut_path, solv, main) in
   if not b1 || not b2 || not b3 then print_endline ("[WARNING] abi/bytecode generation failed - " ^ mut_path)
   else

   let abi_path = Filename.concat (mutant_abi_dir()) (fst ct ^ "." ^ seed_id ^ ".abi") in
   let bin_path = Filename.concat (mutant_bin_dir()) (fst ct ^ "." ^ seed_id ^ ".bin") in
   let bin_runtime_path = Filename.concat (mutant_bin_runtime_dir()) (fst ct ^ "." ^ seed_id ^ ".bin_runtime") in
   let (fp_abi, fp_bin, fp_bin_runtime) = (open_out abi_path, open_out bin_path, open_out bin_runtime_path) in
   Printf.fprintf fp_abi "%s" abi_str;
   Printf.fprintf fp_bin "%s" bin_str;
   Printf.fprintf fp_bin_runtime "%s" bin_runtime_str;
   close_out fp_abi; close_out fp_bin; close_out fp_bin_runtime

let convert_and_apply : Py.Object.t -> Seed.t * BugTemplate.at -> unit
= fun wv (seed,at) ->
  let (main,solv) = BugWorkspace.get_main_solv (Seed.get_path seed) in
  let (pgm,_,global,lines) = preprocess ~is_seed:true (Seed.get_path seed) in
  let res = BugConvert.convert solv global wv pgm at in
  (match res with
   | None -> ()
   | Some ct -> apply_wrapper main solv seed ct lines)

let gen_mutants : BugTemplate.at list -> Seed.t list -> Mutant.t list
= fun ats seeds ->
  print_endline ("[INFO] Generating Mutants ...");
  Py.initialize ~version:3 ();
  let gensim = Py.import "src.bugsynth.py.mygensim" in
  let wv = Py.Module.get_function gensim "load_fastText" [||] in

  let abi_bin_exists sol_path =
    let abi_path =
      Filename.concat (mutant_abi_dir())
      (snd (BatString.replace ~str:(Filename.basename sol_path) ~sub:".sol" ~by:".abi")) in
    let bin_path =
      Filename.concat (mutant_bin_dir())
      (snd (BatString.replace ~str:(Filename.basename sol_path) ~sub:".sol" ~by:".bin")) in
    Sys.file_exists abi_path
    && Sys.file_exists bin_path in

  let pairs = BatList.cartesian_product seeds ats in
  pairs
  |> Os.lwt_list_map ~ncores:!Options.B.ncores (convert_and_apply wv)
  |> (fun _ -> Sys.readdir (mutant_dir())) |> Array.to_list
  |> List.filter (fun s -> BatString.ends_with s ".sol" && abi_bin_exists s)
  |> List.sort Stdlib.compare
  |> List.map (fun s -> Filename.concat (mutant_dir()) s)
  |> List.map Mutant.mk_init_mutant
  |> (fun mutants -> print_endline ("[INFO] # Mutant : " ^ string_of_int (List.length mutants)); mutants)

let get_seeds : string -> Seed.t list
= fun dir ->
  print_endline ("[INFO] seed path : " ^ dir);
  let metafile = meta_path () in
  let rows = Csv.Rows.load ~has_header:true ~header:meta_header metafile in
  let fids = List.map (fun r -> Csv.Row.find r "id") rows in
  let fids = if !Options.B.seed_limit < 0 then fids else BatList.take !Options.B.seed_limit fids in
  let fids =
    let fids2 = Sys.readdir dir |> Array.to_list |> List.filter (fun s -> BatString.ends_with s ".sol") |> List.map (BatString.rchop ~n:4) in
    let pred = (fun fid' -> not (List.mem fid' fids2)) in
    if List.exists pred fids then
      (print_endline ("[WARNING] seed in metafile do not exist: " ^ List.find pred fids);
       List.filter (fun fid' -> List.mem fid' fids2) fids)
    else fids
  in
  let seedfiles = List.map (fun fid -> Filename.concat dir (fid ^ ".sol")) fids in
  let seeds = List.map Seed.mk_init_seed seedfiles in
  print_endline ("[INFO] # Seed : " ^ string_of_int (List.length seeds));
  seeds

let record_progress log logfile =
  (* ref: https://stackoverflow.com/questions/66579013/how-does-a-file-permissions-flag-work-in-ocaml-pervasives *)
  let fp = open_out_gen [Open_append; Open_creat] 0o777 logfile in
  Printf.fprintf fp "%s" (log ^ "\n");
  close_out fp

let do_verify : Seed.t -> Seed.t
= fun seed ->
  let seedfile = Seed.get_path seed in
  let _ = record_progress seedfile (Filename.concat !Options.outdir "progress_seed.txt") in
  let (unproven,time,cmd,err) = try BugValidate.do_verify seedfile with _ -> (print_endline ("My Error @ " ^ seedfile); assert false) in
  let timeout = time > float_of_int (kill_verify_timeout()) in
  let _ = BugValidate.mk_err_report timeout ~filename:seedfile ~errmsg:"Timeout" ~time:time ~reportdir:(verify_report_dir()) in
  { seed with
    cmd = cmd;
    time = time;
    safe = BatSet.is_empty unproven;
    err = not timeout && err;
    timeout = timeout
  }

let verify_seeds : Seed.t list -> Seed.t list (* List.map do_verify seeds *)
= fun seeds ->
  print_endline ("[INFO] Verifying Seeds ...");
  seeds
  |> Os.lwt_list_map ~ncores:!Options.B.ncores do_verify

let validate_mutant : Mutant.t -> Mutant.t
= fun mutant ->
  let mutfile = Mutant.get_path mutant in
  let _ = record_progress mutfile (Filename.concat !Options.outdir "progress_mutant.txt") in
  BugValidate.do_validate mutfile

let validate_mutants mutants : Mutant.t list =
  (* List.filter BugValidate.is_solution mutants *)
  mutants
  |> Os.lwt_list_map ~ncores:!Options.B.ncores validate_mutant

let filter_bugfree_seeds : Seed.t list -> Seed.t list
= fun seeds ->
  print_endline ("[INFO] Identifying bug-free seed list ...");
  let bugfree_file = Filename.concat !Options.input "bugfree_seed.txt" in
  let get_fid path = path |> Filename.basename |> Filename.chop_extension in
  let safe_fids = bugfree_file |> BatFile.lines_of |> BatList.of_enum |> List.map get_fid in
  let is_safe seed = List.mem (get_fid (Seed.get_path seed)) safe_fids in
  List.map (fun seed ->
    {seed with safe = is_safe seed}
  ) seeds

let verify_or_filter_seeds : Seed.t list -> Seed.t list
= fun seeds ->
  let bugfree_fpath = Filename.concat !Options.input "bugfree_seed.txt" in
  if not (Sys.file_exists bugfree_fpath) then
    verify_seeds seeds
  else filter_bugfree_seeds seeds

let run () : at list * Seed.t list * Mutant.t list =
  BugWorkspace.setup ();
  assert (Sys.is_directory (ex_dir()) && Sys.is_directory (seed_dir()) && Sys.is_directory (mutant_dir()));

  (* 1. collect bug-free seeds *)
  let seeds = seed_dir() |> get_seeds |> verify_or_filter_seeds in
  let seeds_safe = List.filter Seed.is_safe seeds in

  (* 2. collect templates *)
  let templates = collect_templates (ex_dir()) in
  if !Options.debug = "bug_at" then debug_ats templates;

  (* 3. generate-and-validate mutants *)
  let mutants = gen_mutants templates seeds_safe in
  let mutants = validate_mutants mutants in
  (templates, seeds, mutants)
