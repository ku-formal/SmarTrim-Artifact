open! Frontend
open Frontend.Lang
open BugTemplate
open BugExtract
open Vocab

(************************************)
(*** Helers for Contextualization ***)
(************************************)

let is_owner gvars x = BatSet.mem x gvars && (Var.origin_name (fst x) = "_owner" || Var.origin_name (fst x) = "owner")
let is_owner_fvar x = (fst x = "owner") && (snd x = Typ.FuncType ([], [EType Address]))
let is_msg_sender x = (fst x = "msg.sender") && (snd x = Typ.EType Address)
let is_msg_sender_fvar x = (fst x = "_msgSender") && (snd x = Typ.FuncType ([], [EType Address]))

(* assume existence of owner var is already checked *)
let find_owner gvars =
  let o = fst (BatSet.find_first (fun x -> Var.origin_name (fst x) = "_owner" || Var.origin_name (fst x) = "owner") gvars) in
  let org = Lv (Var ("_owner", mk_vinfo ~typ:(EType Address) ())) in
  Lv (Var (o, mk_vinfo ~typ:(EType Address) ~org:(Some org) ()))

let owner_fvar =
  let e = Lv (Var ("owner", mk_vinfo ~typ:(FuncType ([],[EType Address])) ())) in
  CallTemp (e,[],None,None,mk_einfo (EType Address))

let msg_sender = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ()))

let msg_sender_fvar =
  let e = Lv (Var ("_msgSender", mk_vinfo ~typ:(FuncType ([],[EType Address])) ())) in
  CallTemp (e,[],None,None,mk_einfo (EType Address))

(*********************************)
(*** Convert abstract Template ***)
(*********************************)

let get_ids_lv : Var.t BatSet.t -> lv -> Var.t BatSet.t * Var.t BatSet.t * Var.t BatSet.t
= fun gvars lv ->
  let ids = var_lv ~fid:true lv in
  let (fids,vars) = BatSet.partition (fun x -> Typ.is_func (snd x)) ids in
  let (g,l) = BatSet.partition (fun x -> BatSet.mem x gvars) vars in
  (g, l, fids)

let get_ids_e : Var.t BatSet.t -> exp -> Var.t BatSet.t * Var.t BatSet.t * Var.t BatSet.t
= fun gvars exp ->
  let ids = var_exp ~fid:true exp in
  let (fids,vars) = BatSet.partition (fun x -> Typ.is_func (snd x)) ids in
  let (g,l) = BatSet.partition (fun x -> BatSet.mem x gvars) vars in
  (g, l, fids)

let get_ids_s : Var.t BatSet.t -> Stmt.t -> Var.t BatSet.t * Var.t BatSet.t * Var.t BatSet.t
= fun gvars stmt ->
  let ids = var_stmt ~fid:true stmt in
  let (fids,vars) = BatSet.partition (fun x -> Typ.is_func (snd x)) ids in
  let (g,l) = BatSet.partition (fun x -> BatSet.mem x gvars) vars in
  (g, l, fids)

let compatible wv (g',l',f') (g,l,f) =
  BugVarMatch.mk_map3 wv (g',g) (l',l) (f',f)

let match_lv wv (lv',g',l',f') (lv,g,l,f) =
  let (b,map) = compatible wv (g',l',f') (g,l,f) in
  (sim_lv lv' lv && b, map)

let match_e wv (e',g',l',f') (e,g,l,f) =
  let (b,map) = compatible wv (g',l',f') (g,l,f) in
  (sim_e e' e && b, map)

let ctx_rep_exp (gvars,e) (gvars_t,_fkey_t,e1,e2) =

  if BatSet.exists (is_owner gvars_t) (var_exp e1)
     && BatSet.exists is_owner_fvar (var_exp ~fid:true e) then
     let owner = find_owner gvars_t in
     let e1' = replace_exp owner owner_fvar e1 in
     let e2' = replace_exp owner owner_fvar e2 in
     (e1',e2')

  else if BatSet.exists is_owner_fvar (var_exp ~fid:true e1)
          && BatSet.exists (is_owner gvars) (var_exp e) then
     let owner = find_owner gvars_t in
     let e1' = replace_exp owner_fvar owner e1 in
     let e2' = replace_exp owner_fvar owner e2 in
     (e1',e2')

  else if BatSet.exists is_msg_sender (var_exp e1)
     && BatSet.exists is_msg_sender_fvar (var_exp ~fid:true e) then
     let e1' = replace_exp msg_sender msg_sender_fvar e1 in
     let e2' = replace_exp msg_sender msg_sender_fvar e2 in
     (e1',e2')

  else if BatSet.exists is_msg_sender_fvar (var_exp ~fid:true e1)
          && BatSet.exists is_msg_sender (var_exp e) then
     let e1' = replace_exp msg_sender_fvar msg_sender e1 in
     let e2' = replace_exp msg_sender_fvar msg_sender e2 in
     (e1',e2')

  else (e1,e2)

let convert_rep_exp gvars wv (func : Func.t) (gvars_t, fkey_t, e1, e2) : ca list =
  let rec scan (stmt : Stmt.t) =
    match stmt with
    | If (e,_s,Revert,ifinfo) ->
      let (e1,e2) = ctx_rep_exp (gvars,e) (gvars_t,fkey_t,e1,e2) in
      let (e1,e2) = ctx_rep_exp (gvars,e) (gvars_t,fkey_t,e1,e2) in

      let loc = ifinfo.if_loc in
      let (g_e1,l_e1,f_e1) = get_ids_e gvars_t e1 in
      let (g_e2,l_e2,f_e2) = get_ids_e gvars_t e2 in
      let (g_e,l_e,f_e) = get_ids_e gvars e in

      let (b1,map1) = match_e wv (e1,g_e1,l_e1,f_e1) (e,g_e,l_e,f_e) in
      let (b2,map2) = compatible wv (g_e2,l_e2,f_e2) (g_e,l_e,f_e) in

      if b1 && b2 then
        let e1' = BugVarMatch.replace_e map1 e1 in
        let e2' = BugVarMatch.replace_e map2 e2 in
        [C_ReplaceExp (loc,e1',e2')]
      else []
    | Seq (s1,s2) -> (scan s1) @ (scan s2)
    | _ -> [] in
  scan func.body

let convert_rep_assign gvars wv (func : Func.t) (gvars_t, _fkey_t, (lv1,e1), (lv2,e2)) : ca list =
  let rec scan (stmt : Stmt.t) =
    match stmt with
    | Assign (lv,e,loc) ->
      let (g_lv1,l_lv1,f_lv1) = get_ids_lv gvars_t lv1 in
      let (g_e1,l_e1,f_e1) = get_ids_e gvars_t e1 in

      let (g_lv2,l_lv2,f_lv2) = get_ids_lv gvars_t lv2 in
      let (g_e2,l_e2,f_e2) = get_ids_e gvars_t e2 in

      let (g_lv,l_lv,f_lv) = get_ids_lv gvars lv in
      let (g_e,l_e,f_e) = get_ids_e gvars e in

      let (b1,map1) = match_lv wv (lv1,g_lv1,l_lv1,f_lv1) (lv,g_lv,l_lv,f_lv) in
      let (b2,map2) = match_e wv (e1,g_e1,l_e1,f_e1) (e,g_e,l_e,f_e) in
      let (b3,map3) = compatible wv (g_lv2,l_lv2,f_lv2) (g_lv,l_lv,f_lv) in
      let (b4,map4) = compatible wv (g_e2,l_e2,f_e2) (g_e,l_e,f_e) in

      if b1 && b2 && b3 && b4 then
        let (lv1', e1') = (BugVarMatch.replace_lv map1 lv1, BugVarMatch.replace_e map2 e1) in
        let (lv2', e2') = (BugVarMatch.replace_lv map3 lv2, BugVarMatch.replace_e map4 e2) in
        [C_ReplaceAssign (loc, (lv1',e1'), (lv2',e2'))]
      else []
    | Seq (s1,s2) -> (scan s1) @ (scan s2)
    | _ -> [] in
  scan func.body

let convert_rm_stmt gvars wv (func : Func.t) (gvars_t, _fkey_t, stmt_t) =
  let rec scan (stmt : Stmt.t) =
    match stmt, stmt_t with
    | If (e,Skip,Revert,ifinfo), Stmt.If (e',Skip,Revert,_) ->
      let loc = ifinfo.if_loc in
      let (g_e',l_e',f_e') = get_ids_e gvars_t e' in
      let (g_e,l_e,f_e) = get_ids_e gvars e in

      let (b,_) = match_e wv (e',g_e',l_e',f_e') (e,g_e,l_e,f_e) in
      if b then [C_RemoveStmt (loc,stmt)]
      else []
    | Seq (s1,s2),_ -> (scan s1) @ (scan s2)
    | _ -> [] in
  scan func.body

let convert_app_unchecked gvars wv (func : Func.t) (gvars_t, _fkey_t, stmt_t) =
  let rec scan stmt =
    match stmt, stmt_t with
    | Stmt.Assign (lv,e,loc), Stmt.Assign (lv',e',_) ->
      let (g_lv',l_lv',f_lv') = get_ids_lv gvars_t lv' in
      let (g_e',l_e',f_e') = get_ids_e gvars_t e' in

      let (g_lv,l_lv,f_lv) = get_ids_lv gvars lv in
      let (g_e,l_e,f_e) = get_ids_e gvars e in

      let (b1,_) = match_lv wv (lv',g_lv',l_lv',f_lv') (lv,g_lv,l_lv,f_lv) in
      let (b2,_) = match_e wv (e',g_e',l_e',f_e') (e,g_e,l_e,f_e) in
      if b1 && b2 then [C_ApplyUnchecked (loc,loc)]
      else []
    | Seq (s1,s2),_ -> (scan s1) @ (scan s2)
    | _ -> [] in
  scan func.body

let convert_rm_modifier _global f (fkey_t, mname_t) : ca list =
  let fkey = Func.fkey f in
  let mod_calls = Func.mod_calls f in
  List.fold_left (fun acc (mcall : Mod_call.t) ->
    if fkey_t = fkey && mname_t = mcall.id then
      acc @ [C_RemoveModifier (mcall.loc, mcall)]
    else acc
  ) [] mod_calls

let remove_all_safemath (func : Func.t) : ca list =
  let rec trans (stmt : Stmt.t) =
    match stmt with
    | Assign (lv,e,_loc) -> (trans_lv lv) @ (trans_exp e)
    | Assume (e,_) -> trans_exp e
    | If (e,s1,s2,_ifinfo) -> (trans_exp e) @ (trans s1) @ (trans s2)
    | While (e,s) -> (trans_exp e) @ (trans s)
    | Unchecked (lst,_) -> List.fold_left (fun acc s -> acc @ (trans s)) [] lst
    | Call (lvop,e,args,ethop,gasop,_loc) ->
      (match lvop with None -> [] | Some lv -> trans_lv lv)
      @ (trans_exp e)
      @ List.fold_left (fun acc e -> acc @ (trans_exp e)) [] args
      @ (trans_eop ethop)
      @ (trans_eop gasop)
    | Seq (s1,s2) -> (trans s1) @ (trans s2)
    | _ -> []

  and trans_exp exp =
    match exp with
    | Lv lv -> trans_lv lv
    | Cast (_,e) -> trans_exp e
    | BinOp (_,e1,e2,_) -> (trans_exp e1) @ (trans_exp e2)
    | UnOp (_,e,_) -> trans_exp e

      (* TODO: consider more safemath *)
    | CallTemp (Lv (MemberAccess (e1,fname,_,_)),[e2],_,_,einfo) ->
      let loc = einfo.loc in
      let inner = (trans_exp e1) @ (trans_exp e2) in
      (match fname with
       | "add" ->
          let exp' = BinOp (Add, e1, e2, einfo) in
          (C_ReplaceExp (loc, exp, exp'))::inner
       | "sub" ->
          let exp' = BinOp (Sub, e1, e2, einfo) in
          (C_ReplaceExp (loc, exp, exp'))::inner
       | "mul" ->
          let exp' = BinOp (Mul, e1, e2, einfo) in
          (C_ReplaceExp (loc, exp, exp'))::inner
       | _ -> inner)

    | CallTemp (e,args,ethop,gasop,_einfo) ->
      (trans_exp e)
      @ List.fold_left (fun acc e -> acc @ (trans_exp e)) [] args
      @ (trans_eop ethop)
      @ (trans_eop gasop)
    | CondTemp (e1,e2,e3,_,_) -> (trans_exp e1) @ (trans_exp e2) @ (trans_exp e3)
    | AssignTemp (lv,e,_) -> (trans_lv lv) @ (trans_exp e)
    | _ -> []

  and trans_eop eop =
    match eop with
    | None -> []
    | Some e -> trans_exp e

  and trans_lv lv =
    match lv with
    | Var _ -> []
    | MemberAccess (e,_,_,_) -> trans_exp e
    | IndexAccess (e,eop,_) -> (trans_exp e) @ (trans_eop eop)
    | Tuple (eops,_) -> List.fold_left (fun acc eop -> acc @ (trans_eop eop)) [] eops

  in trans func.body

let apply_unchecked_to_all (func : Func.t) =
  let rec trans (stmt : Stmt.t) =
    match stmt with
    | Assign (lv,e,loc) ->
      if has_arith_lv lv || has_arith_e e then [C_ApplyUnchecked (loc,loc)]
      else []
    | Assume _ -> assert false
    | If (_e,s1,s2,_ifinfo) -> (trans s1) @ (trans s2) (* TODO: consider e *)
    | While (_e,s) -> trans s (* TODO: consider e *)
    | Unchecked _ -> []
    | Call (_lvop,e,args,ethop,gasop,loc) ->
      if has_arith_e e || List.exists has_arith_e args
         || has_arith_eop ethop || has_arith_eop gasop
        then [C_ApplyUnchecked (loc,loc)]
      else []
    | Seq (s1,s2) -> (trans s1) @ (trans s2)
    | _ -> []

  and has_arith_e exp = (* check whether contain arithmetic ops or not *)
    match exp with
    | Lv lv -> has_arith_lv lv
    | Cast (_,e) -> has_arith_e e
    | BinOp (bop,e1,e2,_) ->
      bop = Add || bop = Sub || bop = Mul || has_arith_e e1 || has_arith_e e2
    | UnOp (_,e,_) -> has_arith_e e
    | IndexRangeAccess (lv,eop1,eop2,_) ->
      has_arith_lv lv || has_arith_eop eop1 || has_arith_eop eop2
    | _ -> false

  and has_arith_eop eop =
    match eop with
    | None -> false
    | Some e -> has_arith_e e

  and has_arith_lv lv =
    match lv with
    | Var _ -> false
    | MemberAccess (e,_,_,_) -> has_arith_e e
    | IndexAccess (e,eop,_) -> has_arith_e e || has_arith_eop eop
    | Tuple (eops,_) -> List.exists has_arith_eop eops

  in trans func.body

let convert_remove_modcalls : Global.t -> Func.t -> string -> ca list
= fun _global f target ->
  let mcalls = Func.mod_calls f in
  List.fold_left (fun acc (mcall : Mod_call.t) ->
    if BatString.starts_with mcall.id target then
      acc @ [C_RemoveModifier (mcall.loc, mcall)]
    else acc
  ) [] mcalls

let rec has_unchecked (stmt : Stmt.t) =
  match stmt with
  | Unchecked _ -> true
  | Seq (s1,s2) -> has_unchecked s1 || has_unchecked s2
  | If (_,s1,s2,_) -> has_unchecked s1 || has_unchecked s2
  | While (_,s) -> has_unchecked s
  | _ -> false

let convert_rm_aguard solv func =
  let sm_reps = remove_all_safemath func in
  if solv < Solc.Ver.mk 0 8 0 then sm_reps
  else
    let sparse_blk = apply_unchecked_to_all func in
    let has_arith_ops = List.length sparse_blk > 0 in
    if (has_arith_ops || List.length sm_reps > 0) then
      if not (has_unchecked func.body) then
        let blk_loc = Func.blk_loc func in
        sm_reps @ [C_ApplyUnchecked (blk_loc, blk_loc)]
      else
        (* TODO: sparsely add unchecked block for sm_reps *)
        sm_reps @ sparse_blk
    else []

let ctx_func_insertion gvars funcs (gvars_t,stmt_t) =

  if BatSet.exists (is_owner gvars_t) (var_stmt stmt_t)
     && not (BatSet.exists (is_owner gvars) gvars)
     && List.exists (fun f -> f.Func.name = "owner" && (Func.param_types f, Func.ret_param_types f) = ([], [EType Address])) funcs then
    let owner = find_owner gvars_t in
    replace_stmt owner owner_fvar stmt_t

  else if BatSet.exists is_owner_fvar (var_stmt ~fid:true stmt_t)
          && BatSet.exists (is_owner gvars) gvars
          && not (List.exists (fun f -> f.Func.name = "owner" && (Func.param_types f, Func.ret_param_types f) = ([], [EType Address])) funcs) then
    let owner = find_owner gvars in
    replace_stmt owner_fvar owner stmt_t

  else stmt_t

let convert_func_insertion (global : Global.t) funcs gvars wv target_line (gvars_t,fname,iparams,rparams,mods_t,stmt,payable) : ca option =
  let stmt = ctx_func_insertion gvars funcs (gvars_t,stmt) in

  (* let modi_to_var = (fun f -> (get_fname f, FuncType (get_param_types f, get_ret_param_types f))) in *)
  let modi_to_var = (fun f -> (f.Func.name, Typ.FuncType ([], []))) in (* TODO *)
  let mods = (Pgm.main global.pgm).funcs |> List.filter Func.is_modifier |> List.map modi_to_var in

  let mods_t = List.map (fun mname -> (mname, Typ.FuncType ([],[]))) mods_t in
  let (b,map) = BugVarMatch.mk_map wv (BatSet.of_list mods_t) (BatSet.of_list mods) in
  if not b then None
  else

  let mods_t' = List.map (fun m -> BatMap.find m map) mods_t |> List.map fst in

  let (g',l',f') = get_ids_s gvars_t stmt in
  let (g,l,f) =
    let funcs = (Pgm.main global.pgm).funcs |> List.filter (fun func -> not (Func.is_external func)) in
    let fids = funcs |> List.map (fun func -> (func.Func.name, Typ.FuncType (Func.param_types func, Func.ret_param_types func))) |> BatSet.of_list in
    (gvars, l', fids) in
  let (b, map) = compatible wv (g', l', f') (g, l, f) in

  if not b then None
  else
  let stmt' = BugVarMatch.replace_s map stmt in
  Some (C_Func (target_line, fname, iparams, rparams, mods_t', stmt', payable))

let convert_aa _solv : Global.t -> Var.t BatSet.t -> Py.Object.t -> Func.t -> Var.t BatSet.t -> aa -> ca list
= fun global gvars wv func gvars_t aa ->
  match aa with
  | A_ReplaceExp (fkey_t,e1,e2) -> convert_rep_exp gvars wv func (gvars_t,fkey_t,e1,e2)
  | A_ReplaceAssign (fkey_t,(lv1,e1),(lv2,e2)) -> convert_rep_assign gvars wv func (gvars_t, fkey_t, (lv1,e1), (lv2,e2))
  | A_RemoveStmt (fkey_t,stmt_t) -> convert_rm_stmt gvars wv func (gvars_t, fkey_t, stmt_t)
  | A_ApplyUnchecked (fkey_t,stmt_t) -> convert_app_unchecked gvars wv func (gvars_t,fkey_t,stmt_t)
  | A_RemoveModifier (fkey_t,mname_t) -> convert_rm_modifier global func (fkey_t,mname_t)
  | A_Func _ -> assert false

  | A_Remove_ModCalls mname -> convert_remove_modcalls global func mname

  | A_Remove_IO_Guard -> assert false
  | A_Decl _ | A_Assert _ -> raise NotImplemented

exception Conversion_Failed

let loop solv global gvars wv funcs (gvars_t, lst) =
  List.fold_left (fun acc aa ->
    match aa with
    | A_Func (fname,iparams,rparams,mods_t,stmt,payable) ->
      let target_line =
        let get_finish_line (f : Func.t) = f.info.floc.finish_line in
        List.fold_left (fun acc f -> max (get_finish_line f) acc) 0 funcs in
      let res = convert_func_insertion global funcs gvars wv target_line (gvars_t,fname,iparams,rparams,mods_t,stmt,payable) in
      (match res with
       | None -> raise Conversion_Failed
       | Some res -> acc @ [res])

    | A_Remove_IO_Guard ->
      let res = List.fold_left (fun acc2 func -> acc2 @ (convert_rm_aguard solv func)) [] funcs in
      if List.length res = 0 then raise Conversion_Failed
      else acc @ res

    | _ ->
      let res = List.fold_left (fun acc2 func -> acc2 @ (convert_aa solv global gvars wv func gvars_t aa)) [] funcs in
      if List.length res = 0 then raise Conversion_Failed
      else
        let main_cname = (Pgm.main global.pgm).name in
        acc @ [if BatString.length main_cname mod 2 = 1 then List.hd res else BatList.last res]
  ) [] lst

let uncompilable_unchecked_blk solv (_tid,ca_lst) =
  if solv >= Solc.Ver.mk 0 8 0 then false
  else
    let is_unchecked ca =
      match ca with
      | C_ApplyUnchecked _ -> true
      | C_Func (_,_,_,_,_,stmt,_) -> has_unchecked stmt
      | _ -> false
    in
    List.exists is_unchecked ca_lst

let convert solv : Global.t -> Py.Object.t -> Pgm.t -> at -> ct option
= fun global wv pgm (tname, gvars_t, lst) ->
  (* accessible gvars: for compilable mutants *)
  let gvars = global.pgm |> Pgm.main |> Contract.accessible_gvars |> BatSet.of_list in
  let funcs = (Pgm.main pgm).funcs in
  try
    let res = loop solv global gvars wv funcs (gvars_t,lst) in
    if not (uncompilable_unchecked_blk solv (tname,res)) then Some (tname,res)
    else None
  with Conversion_Failed -> None
