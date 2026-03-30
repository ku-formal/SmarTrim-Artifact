open! Verify
open! Repair
open! Bugsynth
open! Frontend
open Frontend.Lang
open Vocab
open Options
open Verify.Verifier
open Profiler

let set_default_inline_depth () =
  if !inline_depth < 0 then if !mode = Exploit then inline_depth := 3 else inline_depth := 2 else ()
;;

let preprocess file =
  let lines = List.of_enum (BatFile.lines_of file) in
  Vlang.Z3i.Main.update_timeout !z3timeout;
  set_default_inline_depth ();
  let solc_silent = !Options.verbose = 0 in
  let solv, json_ast = Solc.get_json_ast !solc_ver ~silent:solc_silent file in
  solc_ver := Some solv;
  let pgm = Translator.run json_ast lines in
  main_contract := (Pgm.main pgm).name;
  let pgm = Preprocess.run pgm in
  let pgm = Preprocess2.run pgm in
  let pgm = if !mode = Exploit then Exploit.EPreprocess.run pgm else pgm in
  let pgm = MakeCfg.run pgm in
  let pgm = Inline.run pgm in
  (* inlining is performed on top of an initial cfg. *)
  let global = Global.make_global_info pgm lines in
  (pgm, global, lines)
;;

let record_time () =
  end_cpu := Sys.time () -. !Profiler.start_cpu;
  end_real := Unix.gettimeofday () -. !Profiler.start_real
;;

let print_time () =
  print_endline "";
  print_endline ("Time Elapsed (Real) : " ^ string_of_float !end_real);
  print_endline ("Time Elapsed (CPU)  : " ^ string_of_float !end_cpu)
;;

let do_mode_job (pgm : Pgm.t) (global : Global.t) lines =
  let run_silent = !Options.verbose = 0 in
  let pgm = CallGraph.remove_unreachable_funcs ~silent:run_silent pgm in
  let global = { global with pgm } in
  let paths = Gen_bp.generate ~silent:run_silent global pgm in
  match !mode with
  | Exploit ->
    let db, qmap = Exploit.Run.run global paths in
    Exploit.E_report.report db global qmap stdout;
    record_time ();
    if !mk_report then Exploit.E_report.mk_json_report db global qmap
  | Verify ->
    let () = PatternAnalysis.run global pgm paths in
    let mem = ItvAnalysis.run pgm global paths in
    let global = { global with mem } in
    let qmap, reg_qmap, invmap, specmap = Verifier.run global mem paths in
    Report.print qmap reg_qmap;
    record_time ();
    if !mk_report then Report.mk_json_report global qmap reg_qmap invmap specmap
  | Repair ->
    let () = PatternAnalysis.run global pgm paths in
    let mem = ItvAnalysis.run pgm global paths in
    let global = { global with mem } in
    let result = RunRepair.run global lines pgm in
    record_time ();
    ReportRepair.print result
  | _ -> assert false
;;

let setup_outdir () =
  if !mode = Repair then WorkspaceRepair.setup ()
  else () (* assume outdir already exists in the other modes *)
;;

let main () =
  if !mode = Bugsynth then begin
    let result = Synth.run () in
    record_time ();
    BugReport.print result
  end
  else begin
    activate_default_detector_if_unspecified ();
    print_detector_activation_status ();
    let pgm, global, lines = preprocess !input in
    if !intermediate_language then print_endline (to_string_pgm pgm)
    else if !cfg then print_endline (to_string_cfg_p pgm)
    else
      let () = setup_outdir () in
      do_mode_job pgm global lines
  end
;;

(** make json error file. *)
let json_err ?(detail = "") msg =
  let j =
    Reports.Base_r.make ~filename:!input ~basename:(Filename.basename !input)
      ~cpu_time:(string_of_float !Profiler.end_cpu)
      ~real_time:(string_of_float !Profiler.end_real)
      ~datetime:(Vocab.get_datetime_string !Profiler.start_real)
      ~err_msg:msg
      ~compiler_version:(match !solc_ver with Some v -> Solc.Ver.to_string v | None -> "unknown")
      ~log:(String.split_on_char '\n' detail)
      ()
  in
  let j = Reports.Base_r.(yojson_of_t unit_to_yojson unit_to_yojson j) in
  let base = snd (String.replace ~str:(Filename.basename !input) ~sub:".sol" ~by:"") in
  let fname =
    if !outdir = "" then Os.cat [ Os.project_dir; "output"; base ^ ".json" ]
    else Os.cat [ !outdir; base ^ ".json" ]
  in
  let f = open_out fname in
  Printf.fprintf f "%s" (Yojson.Safe.pretty_to_string j);
  close_out f
;;

let () =
  Climate.Command.run ~version:"250827" ~help_style:Cli.help_style Cli.parser;
  Printexc.record_backtrace true;
  Os.validate_wellformed_argv0 ();
  try
    let () = main () in
    if !verbose > 0 then print_time ();
    if !kill_switch then exit 6 else exit 0
  with exc ->
    begin
      prerr_endline (Printexc.to_string exc);
      prerr_endline (Printexc.get_backtrace ());
      match exc with
      | Solc.CompilationError _versions ->
        if !mk_report then json_err "Compilation Error";
        Options.Exit_code.(exit Compile_error)
      | Solc.UnsupportedSolc _version ->
        if !mk_report then json_err "Unsupported Solc Provided";
        Options.Exit_code.(exit Unsupported_solc)
      | Solution_Inv_Not_Hold ->
        if !mk_report then json_err "Solution Inv Does Not Hold";
        Options.Exit_code.(exit Solution_inv_not_hold)
      | _ ->
        if !mk_report then
          json_err
            ~detail:(Printexc.to_string exc ^ "\n" ^ Printexc.get_backtrace ())
            "Runtime Exception";
        Options.Exit_code.(exit General_error)
    end
;;
