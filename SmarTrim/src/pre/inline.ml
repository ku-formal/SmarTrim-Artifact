open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open Vocab
module C = MakeCfg

let inline_mark = "__inline"
let inline_cnt = ref 0
let update_inline_cnt () = incr inline_cnt
let inline_label () = inline_mark ^ string_of_int !inline_cnt

(** after inlining, there may exist exception node somewhere in the middle. *)
let connect_exception_to_exit (g : Cfg.t) : Cfg.t =
  C.fold_edges
    (fun n1 n2 acc ->
      if Cfg.is_exception_node n1 acc then acc |> C.remove_edge n1 n2 |> C.add_edge n1 Node.exit
      else acc)
    g g
;;

let postprocess (g : Cfg.t) : Cfg.t = g |> connect_exception_to_exit |> C.remove_unreach

let rename_stmt' (callee : Func.t) (gvars : Var.t list) (cnames : string list) (stmt : Stmt.t) :
    Stmt.t =
  let lab = inline_label () in
  match stmt with
  | Return (None, _) -> Skip
  | Return (Some e, loc) ->
    let ret_params = callee.ret_params in
    let lv = params_to_lv ret_params in
    Assign (rename_lv lab gvars lv, rename_e lab gvars e, loc)
  | If _ | Seq _ | While _ | Break | Continue | Unchecked _ -> assert false
  | _ -> rename_stmt lab gvars cnames stmt
;;

let replace_node : Node.t -> Node.t * Stmt.t -> Cfg.t -> Cfg.t =
 fun target (new_node, new_stmt) g ->
  let preds = C.pred target g in
  let succs = C.succ target g in
  let g = C.remove_node target g in
  let g = C.add_node_stmt new_node new_stmt g in
  let g = List.fold_left (fun acc p -> C.add_edge p new_node acc) g preds in
  let g = List.fold_left (fun acc s -> C.add_edge new_node s acc) g succs in
  g
;;

let copy_node : Func.t -> Var.t list -> string list -> Node.t -> Cfg.t -> Cfg.t =
 fun callee gvars cnames node g ->
  if Node.is_entry node || Node.is_exit node then g
  else
    let copied_node = Node.make () in
    let copied_stmt = rename_stmt' callee gvars cnames (C.find_stmt node g) in
    let g' = replace_node node (copied_node, copied_stmt) g in
    {
      g' with
      outpreds_of_lh =
        (if BatSet.mem node g.outpreds_of_lh then BatSet.add copied_node g'.outpreds_of_lh
         else g'.outpreds_of_lh);
      lh_set = (if BatSet.mem node g.lh_set then BatSet.add copied_node g'.lh_set else g'.lh_set);
      lx_set = (if BatSet.mem node g.lx_set then BatSet.add copied_node g'.lx_set else g'.lx_set);
      continue_set =
        (if BatSet.mem node g.continue_set then BatSet.add copied_node g'.continue_set
         else g'.continue_set);
      break_set =
        (if BatSet.mem node g.break_set then BatSet.add copied_node g'.break_set else g'.break_set);
    }
;;

(* returns (entry_node * exit_node * copied graph) *)
let mk_cfg_copy : Func.t -> Var.t list -> string list -> Cfg.t -> Node.t * Node.t * Cfg.t =
 fun callee gvars cnames g ->
  let nodes = Cfg.nodes_of g in
  let g = List.fold_left (fun g' n -> copy_node callee gvars cnames n g') g nodes in
  let entry_node = Node.make () in
  let exit_node = Node.make () in
  let g = replace_node Node.entry (entry_node, Skip) g in
  let g = replace_node Node.exit (exit_node, Skip) g in
  (entry_node, exit_node, g)
;;

(*
let is_recursive_callnode : ctx -> int -> Node.t -> bool
= fun ctx site n ->
  let ctx_n = try BatMap.find n ctx with Not_found -> [] in
  let inlined = List.find_all (fun (l,s') -> site = s') ctx_n in
  List.length inlined >= 2
*)

let is_uint_assign_node n g =
  match C.find_stmt n g with
  | Assign (lv, _, _) when Typ.is_uintkind (get_type_lv lv) -> true
  | _ -> false
;;

let inline_n (gvars : Var.t list) (cnames : string list) (fmap : FuncMap.t) (caller : Func.t)
    (n : Node.t) (g : Cfg.t) : bool * Cfg.t =
  let stmt = C.find_stmt n g in
  match stmt with
  | Call _ when FuncMap.is_undef_call fmap stmt ->
    (* built-in functions *)
    (false, g)
  | Call (lvop, e, args, _, _, loc) when is_internal_call fmap cnames stmt ->
    let cname = caller.info.scope_s in
    let callees = FuncMap.find_matching_funcs cname e (List.map get_type_exp args) cnames fmap in
    assert (BatSet.cardinal callees = 1);
    let callee = BatSet.choose callees in
    let size_of g =
      C.fold_node
        (fun n acc ->
          if (not (Preprocess2.is_pseudo_stmt_node n g)) && is_uint_assign_node n g then acc + 1
          else if is_internal_call_node fmap cnames n g then acc + 1
          else acc)
        g 0
    in
    let _ =
      if !Options.debug = "inline" then
        print_endline (callee.name ^ ", " ^ string_of_int (size_of (Func.cfg callee)))
    in
    let excessive =
      size_of (Func.cfg callee) > 5
      || Cfg.has_loop (Func.cfg callee)
      || BatString.starts_with callee.name "oraclize_"
    in
    if !Options.mode <> Exploit && excessive && not !Options.inline_enforce then (false, g)
    else
      (* Do inlining, if exploit mode or not excessive *)
      let () = update_inline_cnt () in
      let callee_e, callee_x, callee_g = mk_cfg_copy callee gvars cnames (Func.cfg callee) in
      let preds = C.pred n g in
      let succs = C.succ n g in
      let g = C.remove_node n g in
      let input_node = Node.make () in
      let input_stmt =
        (* input_params <- args *)
        try
          let lv = rename_lv (inline_label ()) gvars (params_to_lv callee.params) in
          Stmt.Assign (lv, args_to_exp args, loc)
        with NoParameters -> Skip
      in
      let g = C.add_node_stmt input_node input_stmt g in
      let g = List.fold_left (fun acc p -> C.add_edge p input_node acc) g preds in
      let g = C.add_node_stmt callee_e (C.find_stmt callee_e callee_g) g in
      let g = C.add_edge input_node callee_e g in
      let g =
        G.fold_edges
          (fun src dst acc ->
            let acc = C.add_node_stmt src (C.find_stmt src callee_g) acc in
            let acc = C.add_node_stmt dst (C.find_stmt dst callee_g) acc in
            C.add_edge src dst acc)
          callee_g.graph g
      in
      let ret_node = Node.make () in
      let ret_stmt =
        match lvop with
        | None -> Stmt.Skip
        | Some lv ->
          (* lv <- ret_params when 'lv:= call()' *)
          let e = rename_e (inline_label ()) gvars (Lv (params_to_lv callee.ret_params)) in
          Assign (lv, e, loc)
      in
      let g = C.add_node_stmt ret_node ret_stmt g in
      let g = C.add_edge callee_x ret_node g in
      let g = List.fold_left (fun acc s -> C.add_edge ret_node s acc) g succs in
      let g =
        {
          g with
          outpreds_of_lh = BatSet.union callee_g.outpreds_of_lh g.outpreds_of_lh;
          lh_set = BatSet.union callee_g.lh_set g.lh_set;
          lx_set = BatSet.union callee_g.lx_set g.lx_set;
          continue_set = BatSet.union callee_g.continue_set g.continue_set;
          break_set = BatSet.union callee_g.break_set g.break_set;
        }
      in
      (true, g)
  | _ -> (false, g)
;;

let inline_f ~cnstr_only (gvars : Var.t list) (cnames : string list) (fmap : FuncMap.t) (f : Func.t)
    : bool * Func.t =
  if cnstr_only && not (Func.is_constructor f) then (false, f)
  else
    let g = Func.cfg f in
    let nodes = Cfg.nodes_of g in
    let changed, g' =
      List.fold_left
        (fun (acc_changed, acc_g) n ->
          let new_changed, new_g = inline_n gvars cnames fmap f n acc_g in
          (acc_changed || new_changed, new_g))
        (false, g) nodes
    in
    let g'' = postprocess g' in
    let info = { f.info with cfg = g'' } in
    (changed, Field.fset Func.Fields.info f info)
;;

(* cfg is updated whenever inlining is conducted. *)

let inline_c ~cnstr_only (gvars : Var.t list) (cnames : string list) (fmap : FuncMap.t)
    (c : Contract.t) : bool * Contract.t =
  let funcs = c.funcs in
  let changed, funcs =
    List.fold_left
      (fun (acc_changed, acc_funcs) f ->
        let changed', f' = inline_f ~cnstr_only gvars cnames fmap f in
        (acc_changed || changed', acc_funcs @ [ f' ]))
      (false, []) funcs
  in
  (changed, { c with funcs })
;;

(* inline once *)
let inline_p ~cnstr_only (p : Pgm.t) : bool * Pgm.t =
  let gvars = Pgm.gvars p in
  let fmap = FuncMap.mk_fmap p in
  let cnames = Pgm.cnames p in
  let _base_names, _ = Global.get_basenames p in
  List.fold_left
    (fun (acc_changed, acc_p) c ->
      let changed', c' = inline_c ~cnstr_only gvars cnames fmap c in
      (acc_changed || changed', acc_p @ [ c' ]))
    (false, []) p
;;

let is_target_node cnames fmap n g =
  let stmt = C.find_stmt n g in
  match stmt with
  | Call _ when FuncMap.is_undef_call fmap stmt -> false
  | Call _ -> is_internal_call fmap cnames stmt
  | _ -> false
;;

let remove_call_f cnames fmap f =
  let g = Func.cfg f in
  let g' =
    C.fold_node
      (fun n acc -> if is_target_node cnames fmap n acc then C.add_node_stmt n Revert acc else acc)
      g g
  in
  let g'' = postprocess g' in
  let info = { f.info with cfg = g'' } in
  Field.fset Func.Fields.info f info
;;

let remove_call_c cnames fmap c =
  Field.map Contract.Fields.funcs ~f:(List.map (remove_call_f cnames fmap)) c
;;

let remove_call_p p =
  let cnames = Pgm.cnames p in
  let fmap = FuncMap.mk_fmap p in
  List.map (remove_call_c cnames fmap) p
;;

let rec make_cnstr_fcall_free (p : Pgm.t) : Pgm.t =
  let changed, p' = inline_p ~cnstr_only:true p in
  if changed then make_cnstr_fcall_free p' else p'
;;

let rec inline_ntimes (n : int) (p : Pgm.t) : Pgm.t =
  assert (n >= 0);
  if n = 0 then if !Options.mode = Exploit then p |> make_cnstr_fcall_free |> remove_call_p else p
  else
    let changed, p' = inline_p ~cnstr_only:false p in
    if not changed then p' else inline_ntimes (n - 1) p'
;;

let run (p : Pgm.t) : Pgm.t = inline_ntimes !Options.inline_depth p
