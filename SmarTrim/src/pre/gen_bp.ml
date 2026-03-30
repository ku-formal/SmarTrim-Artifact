open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open MakeCfg
open Vocab

(** returns (processed path set, processing path set, visited root nodes). 'root node' here means
    cutpoint. *)
let gen_onestep_bp_path (cnames : string list) (fmap : FuncMap.t) (g : Cfg.t) (path : Node.t list)
    (visited_roots : Node.t BatSet.t) :
    Node.t list BatSet.t * Node.t list BatSet.t * Node.t BatSet.t =
  let last : Node.t = BatList.last path in
  (* let k = MakeCfg.find_stmt last g in
  Pp.epr "%s: %s@." (Node.to_string last) (to_string_stmt k); *)
  let nexts = succ last g in
  List.fold_left
    (fun (processed, processing, acc_visited_roots) next ->
      if Cfg.is_loophead next g || Node.is_exit next then
        let processed' = BatSet.add (path @ [ next ]) processed in
        let processing' =
          if BatSet.mem next visited_roots then processing else BatSet.add [ next ] processing
        in
        let acc_visited_roots' = BatSet.add next visited_roots in
        (processed', processing', acc_visited_roots')
      else if is_internal_call_node fmap cnames next g then
        let _ = assert (!Options.mode <> Exploit) in
        (* ??? *)
        let processed' = BatSet.add (path @ [ next ]) processed in
        let processing' = BatSet.add (path @ [ next ]) processing in
        (processed', processing', acc_visited_roots)
      else if is_external_call_node next g && !Options.mode = Verify then
        let processed' = BatSet.add (path @ [ next ]) processed in
        let processing' = BatSet.add (path @ [ next ]) processing in
        (processed', processing', acc_visited_roots)
      else (processed, BatSet.add (path @ [ next ]) processing, acc_visited_roots))
    (BatSet.empty, BatSet.empty, visited_roots)
    nexts
;;

(** whenever this function is called, "processed" and "visited_roots" are accumulated, while
    processing is reinitialized *)
let gen_onestep_bp :
    string list ->
    FuncMap.t ->
    Cfg.t ->
    Node.t list BatSet.t * Node.t list BatSet.t * Node.t BatSet.t ->
    Node.t list BatSet.t * Node.t list BatSet.t * Node.t BatSet.t =
 fun cnames fmap g (processed, processing, visited_roots) ->
  BatSet.fold
    (fun path (acc1, acc2, acc3) ->
      let new_processed, new_processing, new_visited_roots =
        gen_onestep_bp_path cnames fmap g path acc3
      in
      ( BatSet.union new_processed acc1,
        BatSet.union new_processing acc2,
        BatSet.union new_visited_roots acc3 ))
    processing
    (processed, BatSet.empty, visited_roots)
;;

let rec fix f cnames fmap func g (processed, processing, visited_roots) =
  let processed', processing', visited_roots' =
    f cnames fmap g (processed, processing, visited_roots)
  in
  if
    BatSet.is_empty processing'
    || !Options.mode = Exploit
       && (not (Func.is_constructor func))
       && BatSet.cardinal processed' >= 50
    (* to prevent out-of-memory *)
  then (processed', processing', visited_roots')
  else if BatSet.cardinal processing' >= 32768 then (BatSet.empty, BatSet.empty, BatSet.empty)
    (* In case of infinite loop; I dont know why, must be fixed *)
  else fix f cnames fmap func g (processed', processing', visited_roots')
;;

let gen_basic_paths_cfg : string list -> FuncMap.t -> Func.t -> Cfg.t -> Node.t list BatSet.t =
 fun cnames fmap func g ->
  let basic_paths, _, _ =
    fix gen_onestep_bp cnames fmap func g
      (BatSet.empty, BatSet.singleton [ Node.entry ], BatSet.singleton Node.entry)
  in
  basic_paths
;;

let rec bfs (g : Cfg.t) (seeds : Node.t BatSet.t) works bps =
  (* works: pending paths *)
  if BatSet.is_empty works (* || (!Options.exploit && BatSet.cardinal bps >= 50) *) then bps
  else
    let (n, path), works = BatSet.pop_min works in
    if Node.is_exit n then bfs g seeds works (BatSet.add path bps)
    else if Cfg.is_loophead n g then
      let nexts = succ n g in
      let works =
        if BatSet.mem n seeds then works
        else List.fold_left (fun acc n' -> BatSet.add (n', [ n; n' ]) acc) works nexts
      in
      let seeds = BatSet.add n seeds in
      bfs g seeds works (BatSet.add path bps)
    else
      let nexts = succ n g in
      let works = List.fold_left (fun acc n' -> BatSet.add (n', path @ [ n' ]) acc) works nexts in
      bfs g seeds works bps
;;

let rec bfs2 (g : Cfg.t) (n : Node.t) (path : Node.t list) : Node.t list BatSet.t =
  if Node.is_exit n then BatSet.singleton path
  else
    let nexts = succ n g in
    List.fold_left (fun acc n' -> BatSet.union (bfs2 g n' (path @ [ n' ])) acc) BatSet.empty nexts
;;

let generate_basic_paths (cnames : string list) (fmap : FuncMap.t) (pgm : Pgm.t) : Pgm.t =
  let func_updator (f : Func.t) =
    let g = Func.cfg f in
    let basic_paths =
      if !Options.pathgen = 1 then gen_basic_paths_cfg cnames fmap f g
      else if !Options.pathgen = 2 then
        bfs g (BatSet.singleton Node.entry)
          (BatSet.singleton (Node.entry, [ Node.entry ]))
          BatSet.empty
      else if !Options.pathgen = 3 then bfs2 g Node.entry [ Node.entry ]
      else failwith "improper path options"
    in
    let cfg = { g with basic_paths } in
    let info = { f.info with cfg } in
    { f with info }
  in
  List.map (Field.map Contract.Fields.funcs ~f:(List.map func_updator)) pgm
;;

(****************************)
(****************************)
(** Collecting Basic Paths **)
(****************************)
(****************************)

let collect_bps_f f : Paths.Set.t =
  let fk = Func.fkey f in
  let bps = (Func.cfg f).basic_paths in
  BatSet.fold (fun bp acc -> Paths.Set.add (Paths.mk fk bp) acc) bps Paths.Set.empty
;;

let collect_bps_c (c : Contract.t) : Paths.Set.t =
  (* modifier themselves are not executable paths *)
  let funcs = List.filter (fun f -> not (Func.is_modifier f)) c.funcs in
  List.fold_left (fun acc f -> Paths.Set.union (collect_bps_f f) acc) Paths.Set.empty funcs
;;

let collect_bps (p : Pgm.t) : Paths.Set.t =
  List.fold_left
    (fun acc c ->
      match !Options.mode with
      | Exploit ->
        if BatString.equal !Options.main_contract c.Contract.name then
          Paths.Set.union (collect_bps_c c) acc
        else acc
      | _ -> Paths.Set.union (collect_bps_c c) acc)
    Paths.Set.empty p
;;

let shuffle_pathids global paths =
  let paths = Paths.Set.to_list paths in
  let paths = Paths.shuffle_id global paths in
  let paths = Paths.Set.of_list paths in
  paths
;;

let generate ?(silent = false) (global : Global.t) (pgm : Pgm.t) : Paths.Set.t =
  if not silent then Profiler.start "[INFO] Generating Paths ... ";
  let cnames = Pgm.cnames pgm in
  let fmap = FuncMap.mk_fmap pgm in
  let pgm = generate_basic_paths cnames fmap pgm in
  let paths = collect_bps pgm in
  let paths = shuffle_pathids global paths in
  if not silent then Profiler.finish "[INFO] Generating Paths ... ";
  if not silent then
    Profiler.print_log ("[INFO] # path : " ^ string_of_int (Paths.Set.cardinal paths));
  if not silent then print_endline "";
  paths
;;
