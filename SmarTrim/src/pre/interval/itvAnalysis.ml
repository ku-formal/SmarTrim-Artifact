open ItvDom
open! Frontend
open Frontend.Lang

(* assign top values to input parameters of public/external functions *)
let update_params : Func.t -> Mem.t -> Mem.t
= fun f mem ->
  match Func.vis f with
  | Public | External ->
    (* For interval domains, always assign top values for parameters of public functions *)
    let params = f.params in
    List.fold_left (fun acc (x,xinfo) ->
      let loc = ItvDom.Loc.of_var x xinfo.vtyp in
      let v = Mem.find loc mem in
      let itop = Itv.top in
      Mem.update loc (Val.update_itv itop v) acc
    ) mem params
  | _ -> mem

let onestep' : Global.t -> string -> Paths.t -> Mem.t -> Mem.t
= fun global main_name path mem ->
  let fk = Paths.fkey path in
  let bp = Paths.basic_path path in
  let func = FuncMap.find fk global.fmap in
  let mem = update_params func mem in
  List.fold_left (fun acc node ->
    ItvSem.eval_stmt global main_name func node acc
  ) mem bp

let onestep : Global.t -> string -> Paths.Set.t -> Mem.t -> Mem.t
= fun global main_name paths mem ->
  Paths.Set.fold (fun path acc ->
    onestep' global main_name path acc
  ) paths mem

let rec fix : Global.t -> string -> int -> Paths.Set.t -> Mem.t -> Mem.t
= fun global main_name cnt paths mem ->
  let mem' = onestep global main_name paths mem in
  let mem' = Mem.widen mem mem' in
    if Mem.le mem' mem then mem'
    else fix global main_name (cnt+1) paths mem'

let weed_out (global : Global.t) (mem : Mem.t) : Mem.t =
  if !Options.intra then Mem.filter (fun k _ -> not (List.mem k global.global_vars)) mem
  else mem

let run : Pgm.t -> Global.t -> Paths.Set.t -> Mem.t 
= fun pgm global paths ->
  let main_name = (Pgm.main pgm).name in
  Profiler.start "[INFO] Performing Interval Analysis ... "; (* DEV *)
  let init = List.fold_left (fun acc g -> Mem.update g (Itv.bot, GTaint.singleton g, BTaint.bot) acc) Mem.bot global.global_vars in 
  let mem = fix global main_name 0 paths init in
  let mem = weed_out global mem in
  Profiler.finish "[INFO] Performing Interval Analysis ... "; (* DEV *)
  if !Options.debug = "itv" || !Options.debug = "itvstop" then (* DEV *)
   (prerr_endline "=== Results  ==="; (* DEV *)
    prerr_endline "Note: The results will be implicitly conjoined with relavant queries."; (* DEV *)
    prerr_endline (Mem.to_string mem); (* DEV *)
    prerr_endline ""); (* DEV *)
  if !Options.debug = "itvstop" then assert false; (* DEV *)
  mem
