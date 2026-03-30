open Options

let ex_dir () = Filename.concat !input "example"
let seed_dir () = Filename.concat !input "seed"

let mutant_dir () = Filename.concat !outdir "mutant"
let mutant_abi_dir () = Filename.concat !outdir "mutant_abi"
let mutant_bin_dir () = Filename.concat !outdir "mutant_bin"
let mutant_bin_runtime_dir () = Filename.concat !outdir "mutant_bin_runtime"

let verify_report_dir () = Filename.concat !outdir "verify_json"
let exploit_report_dir () = Filename.concat !outdir "exploit_json"

let verify_log_dir () = Filename.concat !outdir "verify_log"
let exploit_log_dir () = Filename.concat !outdir "exploit_log"

let ground_truth () = Filename.concat !outdir "ground_truth.csv"

let verify_report_name inputfile =
  Filename.concat
    (verify_report_dir())
    (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".json"))

let exploit_report_name inputfile =
  Filename.concat
    (exploit_report_dir())
    (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".json"))

let verify_log_name inputfile =
  Filename.concat
    (verify_log_dir())
    (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".txt"))

let exploit_log_name inputfile =
  Filename.concat
    (exploit_log_dir())
    (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".txt"))

let meta_path () = Filename.concat !input "meta.csv"

let meta_header =
  ["id"; "duplicate_of"; "actual_order"; "main_name"; "loc"; "address";
   "compiler_version"; "original_compiler_version"; "is_multiple"; "fail"]

let get_org_fid file =
  let lst = BatString.split_on_char '.' (Filename.basename file) in
  BatList.at lst (List.length lst - 2)

let get_main_solv inputfile =
  let metafile = meta_path () in
  if not (Sys.file_exists metafile) then ("", Solc.Ver.mk 0 4 25)
  else
    let fid = get_org_fid inputfile in
    let rows = Csv.Rows.load ~has_header:true ~header:meta_header metafile in
    let row = try List.find (fun r -> fid = Csv.Row.find r "id") rows 
      with Not_found -> List.hd rows 
    in
    List.find (fun solv ->
      BatString.exists (Csv.Row.find row "compiler_version") solv
    ) (List.rev Solc.Ver.(List.map to_string versions))
    |> (fun solv -> (Csv.Row.find row "main_name", Solc.Ver.of_string_exn solv))

let setup () =
  outdir := if !outdir = "" then "./output/bugsynth" else !outdir;
  print_endline ("[INFO] output directory : " ^ !outdir);
  let rm_targets =
    Sys.readdir !outdir
    |> Array.to_list
    |> List.filter (fun s -> not (s = "sillycon.txt"))
    |> List.map (fun s -> Filename.concat !outdir s)
  in
  List.iter (fun d -> assert (Sys.command ("rm -rf " ^ d) = 0)) rm_targets;
  List.iter (fun d -> assert (Sys.command ("mkdir " ^ d) = 0))
  [mutant_dir (); mutant_abi_dir (); mutant_bin_dir (); mutant_bin_runtime_dir ();
   verify_report_dir (); verify_log_dir (); exploit_report_dir (); exploit_log_dir ()]
