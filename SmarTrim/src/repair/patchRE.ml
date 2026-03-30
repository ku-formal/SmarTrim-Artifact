open Patch
open Patch.PatchComp
open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open GenPatchUtil

(********************************)
(** Reentrancy Repair Template **)
(********************************)

type line = int

let rec collect_extcall : Global.t -> Func.t -> Stmt.t -> line * line ->
                          (line * line * line * exp option * Stmt.t) list
= fun global curf stmt (l1,l2) ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (collect_extcall global curf s1 (l1,l2)) @ (collect_extcall global curf s2 (l1,l2))
  | Call (_, Lv (MemberAccess (e,"call",_,_)), _, Some eth, _, loc)
    when Typ.is_address_kind (get_type_exp e) ->
    let vars = var_exp eth in
    if BatSet.disjoint vars (BatSet.of_list global.global_vars) then [(l1,l2, loc.line, Some eth, stmt)]
    else []

    (* to avoid treating undefined function calls as static call *)
  | Call _ when is_undef_call global.fmap stmt -> []

  | Call (_,_,_,_,_,loc) when is_internal_call global.fmap global.cnames stmt ->
    let callee = Global.get_callee_of_intcall global curf stmt in
    if Global.contain_extern global callee then [(l1, l2, loc.line, None, stmt)]
    else []

  | Call _ -> []
  | Extcall _ -> []
  | Skip -> []
  (* | If _ | While _ -> [] *)
  | If (_,s1,s2,ifinfo) ->
    (match ifinfo.if_floc with
     | None -> (collect_extcall global curf s1 (ifinfo.if_tloc.line, ifinfo.if_tloc.finish_line))
     | Some floc ->
       let t_l1 = if ifinfo.if_tloc.line = -1 then l1 else ifinfo.if_tloc.line in
       let t_l2 = if ifinfo.if_tloc.finish_line = -1 then l2 else ifinfo.if_tloc.finish_line in
       let f_l1 = if floc.line = -1 then l1 else floc.line in
       let f_l2 = if floc.finish_line = -1 then l2 else floc.finish_line in
       (collect_extcall global curf s1 (t_l1, t_l2))
        @ (collect_extcall global curf s2 (f_l1, f_l2)))
  | While (_,s) -> collect_extcall global curf s (l1,l2)
  | Break | Continue | Return _ | Revert | Assume _ | Assert _ | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (slst,_) -> List.fold_left (fun acc s -> acc @ (collect_extcall global curf s (l1,l2))) [] slst

(* XXX: This may not be a problem of reentrancy *)
(* XXX: If this is the case, remove template_2. *)
let rec collect_extcall2 : Global.t -> Stmt.t -> (line * exp option * Stmt.t) list
= fun global stmt ->
  match stmt with
  | Seq (s1,s2) -> (collect_extcall2 global s1) @ (collect_extcall2 global s2)
  | Call (_, Lv (MemberAccess (e,"call",_,_)), _, Some eth, _, loc)
    when Typ.is_address_kind (get_type_exp e) ->
    let vars = var_exp eth in
    if BatSet.disjoint vars (BatSet.of_list global.global_vars) then []
    else [(loc.line, Some eth, stmt)]
  | If (_,s1,s2,_) -> (collect_extcall2 global s1) @ (collect_extcall2 global s2)
  | While (_,s) -> collect_extcall2 global s
  | _ -> []

let find_toploc_extcall ~_is_template_1 : Global.t -> Func.t -> (line * exp option * Stmt.t) option
= fun global func ->
  (* let lst = if is_template_1 then collect_extcall global func (get_body func)
            else collect_extcall2 global (get_body func) in *)
  let lst = collect_extcall2 global func.body in
  if List.length lst = 0 then None
  else
    let lst = BatList.sort (fun (a,_,_) (a',_,_) -> Stdlib.compare a a') lst in
    Some (List.hd lst)

(* reeentrancy/simple_dao.sol *)
let add_callret_check line extcall body =
  let rets = callret extcall in
  if not (List.length rets = 1) then []
  else
    let ret = List.hd rets in
    if callret_checked ret body then []
    else [InsertLine (line+1, Assume (Lv ret, Loc.dummy), false)]

let template_1 : Global.t -> Func.t list -> patch_comp list
= fun global funcs ->
  List.fold_left (fun acc func ->
    let finfo = func.Func.info in
    (* let _ = print_endline (get_fname func ^ " : " ^  string_of_int finfo.floc.line ^ " , " ^ string_of_int finfo.floc.finish_line) in *)
    let extcalls = collect_extcall global func func.body (finfo.floc.line, finfo.floc.finish_line) in
    List.fold_left (fun acc2 (l1,l2,line,_,extcall) ->
      let triples = get_assigns line func.body in
      let triples = List.filter (fun (_,_,c) ->
         (* let _ = print_endline (to_string_stmt (Assign (a,b,dummy_loc))) in
            let _ = print_endline (string_of_int c ^ "," ^ string_of_int l1 ^ "," ^ string_of_int l2) in
            let _ = print_endline "" in *)
         l1 <= c && c <= l2) triples in
      let assigns = List.map (fun (a,b,c) -> (c, Stmt.Assign (a,b,Loc.dummy))) triples in
      if List.length assigns = 0 then acc2 (* line 19 of reentrancy_bonus.sol *)
      else
        acc2 @ [Move (assigns, line, extcall)] @ (add_callret_check line extcall func.body)
    ) [] extcalls
    |> (fun res ->
        if List.length res = 0 then acc
        else
          let ifs = get_if func.body in
          let ifs = List.map (fun (a,b,c,d) -> ElseRevert (a,b,c,d)) ifs in
          acc @ res @ ifs)
          (* acc @ [AtomLst (res@ifs)]) *)
  ) [] funcs
  |> (fun res ->
      if List.length res = 0 then []
      else [AtomLst res])

let template_2 : Global.t -> Func.t list -> patch_comp list
= fun global funcs ->
  List.fold_left (fun acc func ->
    match find_toploc_extcall ~_is_template_1:false global func with
    | None -> acc
    | Some (_, None, _) -> assert false
    | Some (line, Some eth, extcall) ->
      let assign = get_assigns line func.body in
      if List.length assign > 1 then acc
      else if List.length assign = 0 then acc (* 0x0eb68f34efa0086e4136bca51fc4d0696580643e *)
      else
        let _ = assert (List.length assign = 1) in
        let (lv',e',line') = List.hd assign in
        if not (to_string_exp eth = to_string_exp (Lv lv')) then acc
        else
          (* x.call.value(b[m]); b[m] = 0 *)
          (* ~> tmp = b[m]; b[m] = 0; x.call.value(tmp); *)
          let tmp = fresh_tmp () in
          let tmp = Var (tmp, mk_vinfo ~typ:(get_type_exp eth) ()) in
          let a1 = Replace (line, eth, Lv tmp) in
          let a2 =
            let assign = (line', Stmt.Assign (lv', e', Loc.dummy)) in (* assign behind the extcall *)
            Move ([assign], line, extcall) in
          let a3 = InsertLine (line, Assign (tmp, Lv lv', Loc.dummy), false) in
          acc @ [AtomLst [a1;a2;a3]]
  ) [] funcs

let is_global_based_addr_typed_exp : Global.t -> exp -> bool
= fun global exp ->
  match exp with
  | Lv (Var (v,vinfo)) ->
    (* is_address vinfo.vtyp && List.mem (v,vinfo.vtyp) global.gvars *)
    Typ.is_address_kind vinfo.vtyp && BatSet.cardinal (ItvDom.Val.gtaint_of (ItvDom.Mem.find (v,vinfo.vtyp) global.mem)) > 0
  | Lv (IndexAccess (Lv (Var (v,vinfo)),_,typ)) ->
    (* is_address typ && List.mem (v,vinfo.vtyp) global.gvars *)
    Typ.is_address_kind typ && BatSet.cardinal (ItvDom.Val.gtaint_of (ItvDom.Mem.find (v,vinfo.vtyp) global.mem)) > 0
  | Lv (MemberAccess (_,_,_,_)) -> false (* field access is likely to be unprivileged access *)
  | _ -> false

let has_addr_comparison_in_assume : Global.t -> Func.t -> bool
= fun global f ->
  let g = Func.cfg f in
  let nodes = MakeCfg.nodesof g in
  List.exists (fun n ->
    let stmt = Cfg.find_stmt n g in
    match stmt with
    | Assume (BinOp (_,e1,e2,_), _) ->
      is_global_based_addr_typed_exp global e1
      || is_global_based_addr_typed_exp global e2
    | _ -> false
  ) nodes

let debug_template ext_and_states ext_and_states_wo_call ext_and_states_wo_mod ext_and_states_wo_mod2 =
  let get_fname f = f.Func.name in
  let _ = print_endline ("group 1 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states) in
  let _ = print_endline ("group 2 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_call) in
  let _ = print_endline ("group 3 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_mod) in
  let _ = print_endline ("group 4 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_mod2) in
  assert false

(* Add 'nonReentrant' Modifier *)
let template_3 : Global.t -> Func.t list -> patch_comp list
= fun global funcs ->
  let funcs = List.filter Func.(fun f -> not (is_constructor f) && not (is_modifier f) && not (is_view_pure f)) funcs in

  (* for search efficiency and patch readability *)
  (* see withdraw(external) vs. send(internal) of re/0x1223730bae7fd3d2fe1e7948b0a90c8a9f3b8446_1.sol *)
  let funcs = List.filter Func.(fun f -> is_public f || is_external f) funcs in

  let ext = List.filter (fun f -> Global.contain_extern global f) funcs in
  (* let _ = print_endline (Vocab.string_of_list get_fname ext) in *)
  let states = List.filter (PatternAnalysis.may_update_state_f global) funcs in

  let states_wo_mod = List.filter (fun f -> not (has_addr_comparison_in_assume global f)) states in

  let ext_and_states = BatList.unique ~eq:(fun f1 f2 -> Func.fkey f1 = Func.fkey f2) (ext@states) in                  (* X + S *)
  let ext_and_states_wo_call = List.filter (fun f -> not (Global.contain_dead_intcall global f)) ext_and_states in         (* (X + S) \ call *)
  let ext_and_states_wo_mod = BatList.unique ~eq:(fun f1 f2 -> Func.fkey f1 = Func.fkey f2) (ext@states_wo_mod) in    (* X + (S/M) *)
  let ext_and_states_wo_mod2 =  List.filter (fun f -> not (Global.contain_dead_intcall global f)) ext_and_states_wo_mod in (* (X + (S/M)) \ call *)

  let _ = if !Options.debug = "fix_re" then debug_template ext_and_states ext_and_states_wo_call ext_and_states_wo_mod ext_and_states_wo_mod2 in

  let comps1 = List.map (fun f -> AddModifier (Func.fkey f, "@nonReentrant", f.info.param_loc.finish_line)) ext_and_states in
  let comps2 = List.map (fun f -> AddModifier (Func.fkey f, "@nonReentrant", f.info.param_loc.finish_line)) ext_and_states_wo_call in
  let comps3 = List.map (fun f -> AddModifier (Func.fkey f, "@nonReentrant", f.info.param_loc.finish_line)) ext_and_states_wo_mod in
  let comps4 = List.map (fun f -> AddModifier (Func.fkey f, "@nonReentrant", f.info.param_loc.finish_line)) ext_and_states_wo_mod2 in

  let r1 = if List.length comps1 > 0 then [AtomLst comps1] else [] in
  let r2 = if List.length comps2 > 0 then [AtomLst comps2] else [] in
  let r3 = if List.length comps3 > 0 then [AtomLst comps3] else [] in
  let r4 = if List.length comps4 > 0 then [AtomLst comps4] else [] in
  r1 @ r2 @ r3 @r4

let generate : Global.t -> Func.t list -> patch_comp list
= fun global funcs ->
  (template_1 global funcs)
  @ (template_2 global funcs)
  @ (template_3 global funcs)
