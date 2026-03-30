open! Frontend
open Frontend.Lang
open BugTemplate
open Vocab

(* check if given two exps are similar *)
let rec sim_e : exp -> exp -> bool
= fun exp1 exp2 ->
  if to_string_exp exp1 = to_string_exp exp2 then true (* branch due to msg.sender *)
  else
  match exp1, exp2 with
  | V Int _, V Int _ -> true
  | V Frac _, V Frac _ -> true
  | V Bool true, V Bool true -> true
  | V Bool false, V Bool false -> true
  | Str _, Str _ -> true
  | Lv lv, Lv lv' -> sim_lv lv lv'
  | Cast (_,e), Cast (_,e') -> sim_e e e'
  | BinOp (_,e1,e2,_), BinOp (_,e1',e2',_) -> sim_e e1 e1' && sim_e e2 e2'
  | UnOp (uop,e,_), UnOp (uop',e',_) -> uop=uop' && sim_e e e'
  | ETypeName _, ETypeName _ -> true
  | IndexRangeAccess (lv,sop,fop,_), IndexRangeAccess (lv',sop',fop',_) ->
    sim_lv lv lv' && sim_eop sop sop' && sim_eop fop fop'
  | TypeInfo _, TypeInfo _ -> true
  | IncTemp (e,_,_), IncTemp (e',_,_) -> sim_e e e'
  | DecTemp (e,_,_), DecTemp (e',_,_) -> sim_e e e'
  | CallTemp (Lv (MemberAccess (e1,_,_,_)),[e2],_,_,_), BinOp (_,e1',e2',_) ->
    sim_e e1 e1' && sim_e e2 e2'
  | CallTemp (e,args,_,_,_), CallTemp (e',args',_,_,_)
    when to_string_exp e = to_string_exp e' && List.length args = List.length args' ->
    List.for_all2 sim_e args args'
  | CondTemp (e1,e2,e3,_,_), CondTemp (e1',e2',e3',_,_) ->
    sim_e e1 e1' && sim_e e2 e2' && sim_e e3 e3'
  | AssignTemp (lv,e,_), AssignTemp (lv',e',_) ->
    sim_lv lv lv' && sim_e e e'

  | Lv (Var ("msg.sender",_)), CallTemp (Lv (Var ("_msgSender",_)),[],_,_,_)
  | CallTemp (Lv (Var ("_msgSender",_)),[],_,_,_), Lv (Var ("msg.sender",_)) -> true
  | Lv (Var (x,_)), CallTemp (Lv (Var ("owner",_)),[],_,_,_)
  | CallTemp (Lv (Var ("owner",_)),[],_,_,_), Lv (Var (x,_)) 
    when Var.origin_name x = "owner" || Var.origin_name x = "_owner" -> true
  | _ -> false

and sim_eop : exp option -> exp option -> bool
= fun eop1 eop2 ->
  match eop1, eop2 with
  | Some e1, Some e2 -> sim_e e1 e2
  | None, None -> true
  | _ -> false

and sim_lv : lv -> lv -> bool
= fun lv1 lv2 ->
  match lv1, lv2 with
    (* compare original name to address local vars with different ids *)
  | Var (x,info), Var (x',info') -> Var.org (x,info.vtyp) = Var.org (x',info'.vtyp)
  | MemberAccess (e,_,_,_), MemberAccess (e',_,_,_) -> sim_e e e' (* && x = x' && *)
  | IndexAccess (e1,Some _,_), IndexAccess (e1',Some _,_) -> sim_e e1 e1'
  | _ -> false

(* check if given two stmts are similar *)
let rec sim_s : Stmt.t -> Stmt.t -> bool
= fun stmt1 stmt2 ->
  match stmt1,stmt2 with
  | Assign (lv,_,_), Assign (lv',_,_) -> sim_lv lv lv'
  | Assume (e,_), Assume (e',_) -> sim_e e e'
  | Unchecked (lst1,_), Unchecked (lst2,_) when List.length lst1 = List.length lst2 ->
    List.for_all2 sim_s lst1 lst2
  | Seq (s1,s2), Seq (s1',s2') -> sim_s s1 s1' && sim_s s2 s2'

  (* TODO: need to trim. E.g., small changes in s1 or s2 *)
  | If (e,_s1,_s2,_), If (e',_s1',_s2',_) -> sim_e e e'
  | While (e,_s), While (e',_s') -> sim_e e e'

  | _ -> false

let get_loc_s : Stmt.t -> Loc.t
= fun stmt ->
  match stmt with
  | Assign (_,_,loc) -> loc
  | Assert (_,_,loc) -> loc
  | Unchecked (_,loc) -> loc
  | If (_,_,_,ifinfo) -> ifinfo.if_loc
  | _ -> Loc.dummy

(**************************)
(*** Differentiate body ***)
(**************************)

let partition_gvars_lvars : Global.t -> Var.t BatSet.t -> (Var.t BatSet.t * Var.t BatSet.t)
= fun global xs ->
  let gvars = global.global_vars in
  BatSet.partition (fun x -> List.mem x gvars) xs

(* TODO: extend *)
let rec diff_e : Global.t -> Fkey.t -> exp -> exp -> aa list
= fun global fkey exp1 exp2 ->
  match exp1, exp2 with
  | _ when to_string_exp ~report:true exp1 = to_string_exp ~report:true exp2 -> []
  | Lv lv1, Lv lv2 -> diff_lv global fkey lv1 lv2
  | BinOp (bop,e1,e2,_), BinOp (bop',e1',e2',_) ->
    if bop = bop' then
      (diff_e global fkey e1 e1') @ (diff_e global fkey e2 e2')
    else [A_ReplaceExp (fkey,exp1,exp2)]
  | _ -> []

and diff_lv : Global.t -> Fkey.t -> lv -> lv -> aa list
= fun _global fkey lv1 lv2 ->
  if to_string_lv ~report:true lv1 = to_string_lv ~report:true lv2 then []
  else [A_ReplaceExp (fkey, Lv lv1, Lv lv2)]

let diff_assign _global fkey (lv1,e1,_loc1) (lv2,e2,_loc2) =
  [A_ReplaceAssign (fkey, (lv1,e1), (lv2,e2))]

let rec to_lst (stmt : Stmt.t) =
  match stmt with
  | Seq (s1,s2) -> (to_lst s1) @ (to_lst s2)
  | _ -> [stmt]

let sim_blk : Stmt.t list -> Stmt.t list -> bool
= fun lst1 lst2 ->
  let _ = assert (List.length lst1 = List.length lst2) in
  List.for_all2 sim_s lst1 lst2

let sim_sub_blk : Stmt.t list -> Stmt.t list -> bool
= fun lst1 lst2 ->
  List.length lst2 <= List.length lst1
  && sim_blk (BatList.take (List.length lst2) lst1) lst2

let is_unchecked (stmt : Stmt.t) =
  match stmt with Unchecked _ -> true | _ -> false

let rec diff_blk : Global.t -> Fkey.t -> Stmt.t list -> Stmt.t list -> aa list
= fun global fkey lst1 lst2 ->
  match lst1,lst2 with
  (* to exclude dummy statements for some modeling *)
  | h1::t1,_ when get_loc_s h1 = Loc.dummy -> diff_blk global fkey t1 lst2
  | _,h2::t2 when get_loc_s h2 = Loc.dummy -> diff_blk global fkey lst1 t2

  | [], [] -> []
  | [], _::_ -> raise NotImplemented
    (* (insert_s global fsig params h) :: (diff_blk global (ff,fsig,params) lst1 t) *)

  | _::_, [] -> List.map (fun s -> A_RemoveStmt (fkey, s)) lst1

  | h1::_, (Unchecked (lst,_))::t2
    when not (is_unchecked h1) && sim_sub_blk lst1 lst ->
    let (front,back) = BatList.split_at (List.length lst) lst1 in
    let res1 = List.map (fun s -> A_ApplyUnchecked (fkey, s)) front in
    let res2 = diff_blk global fkey front lst in
    let res3 = diff_blk global fkey back t2 in
    res2 @ res3 @ res1
    (* res1 @ res2 @ res3 *)

  | h1::t1, h2::t2 ->
    if to_string_stmt ~report:true h1 = to_string_stmt ~report:true h2 then
      diff_blk global fkey t1 t2
    else if sim_s h1 h2 then
      (diff_s global fkey h1 h2) @ (diff_blk global fkey t1 t2)
    else
      raise NotImplemented
      (* (insert_s global fsig params h2)::(diff_blk global (ff,fsig,params) lst1 t2) *)
      (* (C_InsertStmt (get_loc_s h1, h2))::(diff_blk global ff lst1 t2) *)

and diff_s : Global.t -> Fkey.t -> Stmt.t -> Stmt.t -> aa list
= fun global fkey stmt1 stmt2 ->
  let _ = assert (to_string_stmt ~report:true stmt1 <> to_string_stmt ~report:true stmt2) in
  match stmt1, stmt2 with
  | Assign (lv,e,loc), Assign (lv',e',loc') when sim_lv lv lv' ->
    diff_assign global fkey (lv,e,loc) (lv',e',loc')
    (* diff_e ff exp exp' *)

  | Assume (e,_), Assume (e',_) -> diff_e global fkey e e'
  | If (e,s1,s2,_), If (e',s1',s2',_) ->
    let at1 = diff_e global fkey e e' in
    let at2 = diff_blk global fkey (to_lst s1) (to_lst s1') in
    let at3 = diff_blk global fkey (to_lst s2) (to_lst s2') in
    at1 @ at2 @ at3
  | While (_e,s), While (_e',s') ->
    (* (diff_e e e') @ *) (diff_blk global fkey (to_lst s) (to_lst s'))
  | Unchecked (lst1,_), Unchecked (lst2,_)
    when List.length lst1 = List.length lst2 ->
    diff_blk global fkey lst1 lst2
  | Seq _, Seq _ -> assert false
  | _ -> assert false

let diff_body : Global.t -> Fkey.t -> Stmt.t -> Stmt.t -> aa list
= fun global fkey org mut ->
  diff_blk global fkey (to_lst org) (to_lst mut)

(************************************)
(*** Differentiate modifier calls ***)
(************************************)

let get_modifiers_of_func (global : Global.t) f =
  Func.mod_calls f
  |> List.map Mod_call.id
  |> List.map (fun name -> FuncMap.find_modifier_by_name global.fmap name)

let diff_mod : Global.t -> Fkey.t -> Func.t -> Func.t -> aa list
= fun global fkey org_f mut_f ->
  let org_mods = get_modifiers_of_func global org_f in
  let mut_mods = get_modifiers_of_func global mut_f in
  let diff_mods = List.filter (fun org_m -> not (List.mem (org_m.Func.name) (List.map (fun f -> f.Func.name) mut_mods))) org_mods in
  List.map (fun m -> A_RemoveModifier (fkey, m.Func.name)) diff_mods

(************)
(*** Core ***)
(************)

let matched o m =
  let o, m = Func.fkey o, Func.fkey m in
  snd (BatString.replace ~str:o.func ~sub:"org_" ~by:"") = snd (BatString.replace ~str:m.func ~sub:"mut_" ~by:"")
  && o.param_typs = m.param_typs

let align_org_mut : Global.t -> Func.t list -> (Func.t * Func.t) list
= fun _global funcs ->
  let (org_funcs, mut_funcs) =
    let pred prefix f = BatString.starts_with f.Func.name prefix in
    let compare f1 f2 = Stdlib.compare (Func.fkey f1) (Func.fkey f2) in
    (funcs |> List.filter (pred "org_") |> List.sort compare,
     funcs |> List.filter (pred "mut_") |> List.sort compare)
  in
  List.fold_left2 (fun acc o m ->
    if not (matched o m) then (prerr_endline "match between original and mutant functions failed."; exit 1)
    else acc @ [(o,m)]
  ) [] org_funcs mut_funcs

let diff_f : Global.t -> aa list -> (Func.t * Func.t) -> aa list
= fun global acc (org,mut) ->
  let fkey = Func.fkey org in
  acc
  @ (diff_mod global fkey org mut)
  @ (diff_body global fkey (org.body) (mut.body))

let extract_mutation_ops : Global.t -> Func.t list -> aa list
= fun global funcs ->
  let pairs = align_org_mut global funcs in
  List.fold_left (diff_f global) [] pairs

let extract_inserted_funcs : Global.t -> Func.t list -> aa list
= fun global funcs ->
  let pred = (fun f -> BatString.starts_with f.Func.name "sillycon_insert_") in
  let funcs = List.filter pred funcs in
  let get_arg f =
    let get_fname f = snd (BatString.replace ~str:f.Func.name ~sub:"sillycon_insert_" ~by:"") in
    let get_param_vars f = f |> Func.param_vars |> List.map Var.org in
    let get_ret_param_vars f = f |> Func.ret_param_vars |> List.map Var.org in
    let get_mods f =
      let remove_org fname = snd (BatString.replace ~str:fname ~sub:"org_" ~by:"") in
      let remove_mut fname = snd (BatString.replace ~str:fname ~sub:"mut_" ~by:"") in
      f |> get_modifiers_of_func global |> List.map (fun f -> f.Func.name) |> List.map remove_org |> List.map remove_mut in
    (get_fname f, get_param_vars f, get_ret_param_vars f, get_mods f, Func.body f, Func.is_payable f)
  in
  List.map (fun f -> A_Func (get_arg f)) funcs

(*************************)
(*** general templates ***)
(*************************)

let add_remove_modcalls funcs acc =
  List.fold_left (fun acc f ->
    if not (BatString.starts_with f.Func.name "sillycon_remove_mod_") then acc
    else
      let mname = BatString.lchop ~n:(BatString.length "sillycon_remove_mod_") f.Func.name in
      acc @ [A_Remove_ModCalls mname]
  ) acc funcs

let exist_remove_io_guard funcs =
  let pred f = (f.Func.name = "sillycon_remove_io_guard") in
  List.exists pred funcs

let add_remove_io_guard acc = acc @ [A_Remove_IO_Guard]

let extract_general_templates funcs =
  []
  |> add_remove_modcalls funcs
  |> (if exist_remove_io_guard funcs then add_remove_io_guard else Vocab.id)

let extract : Global.t -> string -> Pgm.t -> at
= fun global tname pgm ->
  let funcs = (Pgm.main pgm).funcs in
  let gvars = BatSet.of_list global.global_vars in
  ((extract_general_templates funcs)
  @ (extract_mutation_ops global funcs)
  @ (extract_inserted_funcs global funcs))
  |> (fun res -> (tname, gvars, res))
