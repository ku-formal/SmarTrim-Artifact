open! Frontend
open Frontend.Lang

let tmp_cnt = ref 0
let fresh_tmp () = (tmp_cnt := !tmp_cnt + 1; "tmp__" ^ string_of_int !tmp_cnt)

let rec get_assigns : int -> Stmt.t -> (lv * exp * int) list
= fun l stmt ->
  match stmt with
  | Assign (lv,_,_)
    when BatString.starts_with (to_string_lv lv) Translator.param_name -> [] (* exclude assignments for normalizing return statements *)
  | Assign (lv,e,loc)
    when loc.line > l        (* collect assignments after the target external call *)
         && loc.line > 0     (* exclude modelled assignments, see frontend/preprocess2.ml *)
         && not (loc.line=l) (* remove intermediate assignmetns generated from external call *)
                                   (* E.g., bool ret = msg.call.value .. => Tmp = msg.call.value ..; "ret = Tmp" *)
    -> [(lv, e, loc.line)]
  | Assign _ -> []
  | Decl _ -> []
  | Seq (s1,s2) -> (get_assigns l s1) @ (get_assigns l s2)
  | Call _ | Extcall _ | Skip -> []
  | If (_,s1,s2,_) -> (get_assigns l s1) @ (get_assigns l s2)
(*    let lst1, lst2 = get_assigns l s1, get_assigns l s2 in
    if List.length lst1 > 0 && List.length lst2 = 0 then lst1
    else if List.length lst1 = 0 && List.length lst2 > 0 then lst2
    else [] *)
  | While (_,_) -> [] (* get_assigns l s *)
  | Break | Continue | Return _ | Revert
  | Assume _ | Assert _ | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (blk,_) -> List.fold_left (fun acc s -> acc @ (get_assigns l s)) [] blk

let rec callret : Stmt.t -> lv list
= fun stmt ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (callret s1) @ (callret s2)
  | Call (lvop, Lv (MemberAccess (e,"call",_,_)), _args, Some _eth, _gasop, _loc)
    when Typ.is_address_kind (get_type_exp e) ->
    (match lvop with Some lv -> [lv] | None -> [])
  | Call _ | Extcall _ -> []
  | Skip -> []
  | If (_,s1,s2,_) -> (callret s1) @ (callret s2)
  | While (_,s) -> callret s
  | Break | Continue | Return _ | Revert
  | Assume _ | Assert _ | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (blk,_) ->
    List.fold_left (fun acc s ->
      acc @ (callret s)
    ) [] blk

let rec callret_checked : lv -> Stmt.t -> bool
= fun ret stmt ->
  match stmt with
  | Assign _ | Decl _ -> false
  | Seq (s1,s2) -> callret_checked ret s1 || callret_checked ret s2
  | Call _ | Extcall _ -> false
  | If (e,s1,s2,_) ->
    let ret = match ret with Tuple ([Some (Lv ret);None],_) -> ret | _ -> ret in
    if BatString.exists (to_string_exp e) (to_string_lv ret) then true
    else callret_checked ret s1 || callret_checked ret s2
  | Assume (e,_) ->
    let ret = match ret with Tuple ([Some (Lv ret);None],_) -> ret | _ -> ret in
    BatString.exists (to_string_exp e) (to_string_lv ret)
  | Skip -> false
  | While (e,s) ->
    if to_string_exp e = to_string_lv ret then true
    else callret_checked ret s
  | Break | Continue | Return _ | Revert
  | Assert _ | Assembly _ | Placeholder | Label _ -> false
  | Unchecked (blk,_) ->
    List.exists (fun s -> (callret_checked ret s)) blk

let is_if_without_else : Stmt.t -> bool
= fun stmt ->
  match stmt with
  | If (_,_,_,ifinfo) ->
    (match ifinfo.if_floc with None -> true | Some _ -> false)
  | _ -> assert false

let rec get_if' : lv list -> Stmt.t -> (exp * Stmt.t * Stmt.t * Ifinfo.t) list
= fun rets stmt ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (get_if' rets s1) @ (get_if' rets s2)
  | Call _ | Extcall _ | Skip -> []
  | If (_,_,_,i) when i.if_loc.line < 0 -> []
  | If (e,s1,s2,i) ->
    if is_if_without_else stmt && List.mem (to_string_exp e) (List.map to_string_lv rets)
      then [(e,s1,s2,i)] @ (get_if' rets s1) @ (get_if' rets s2)
    else (get_if' rets s1) @ (get_if' rets s2)
  | While (_,s) -> get_if' rets s
  | Break | Continue | Return _ | Revert
  | Assume _ | Assert _ | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (blk,_) ->
    List.fold_left (fun acc s ->
      acc @ (get_if' rets s)
    ) [] blk

let get_if : Stmt.t -> (exp * Stmt.t * Stmt.t * Ifinfo.t) list
= fun stmt -> get_if' (callret stmt) stmt

let rec is_safe' : exp -> bool
= fun exp ->
  let is_address = Typ.is_address in
  match exp with
  | BinOp (Eq,e1,e2,_)
    when not (is_address (get_type_exp e1)) && not (is_address (get_type_exp e2)) -> true
  | BinOp (Eq,e1,e2,_)
    when is_address (get_type_exp e1) && is_address (get_type_exp e2)
         && (to_string_exp e1 = "msg.sender" || to_string_exp e2 = "msg.sender") -> true

  | BinOp (LOr,e1,e2,_) -> is_safe' e1 && is_safe' e2
  | BinOp (LAnd,e1,e2,_) -> is_safe' e1 && is_safe' e2
  | _ -> false

let is_safe_or : exp -> bool
= fun exp ->
  match exp with
  | BinOp (LOr,e1,e2,_) -> is_safe' e1 && is_safe' e2
  | _ -> false

(* (stmt_loc, exp) *)
let rec collect_exp : Stmt.t -> (Loc.t * exp) list
= fun stmt ->
  let is_address = Typ.is_address in
  match stmt with
  | Assign (lv,e,loc) -> (exp_lv loc lv) @ (exp_e loc e)
  | Decl _ -> []
  | Seq (s1,s2) -> (collect_exp s1) @ (collect_exp s2)
  | Call (lvop,e,args,ethop,gasop,loc) ->
    let lst1 = match lvop with None -> [] | Some lv -> exp_lv loc lv in
    let lst2 = exp_e loc e in
    let lst3 = List.fold_left (fun acc e' -> acc @ (exp_e loc e')) [] args in
    let lst4 = match ethop with None -> [] | Some e' -> exp_e loc e' in
    let lst5 = match gasop with None -> [] | Some e' -> exp_e loc e' in
    lst1 @ lst2 @ lst3 @ lst4 @ lst5
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (id, is_static, fkey);
    let lst1 = match ret with None -> [] | Some lv -> exp_lv loc lv in
    let lst2 = exp_e loc target in
    let lst3 = List.fold_left (fun acc e' -> acc @ (exp_e loc e')) [] args in
    let lst4 = match ether with None -> [] | Some e' -> exp_e loc e' in
    let lst5 = match gas with None -> [] | Some e' -> exp_e loc e' in
    lst1 @ lst2 @ lst3 @ lst4 @ lst5
  | Skip -> []
  | If (BinOp (Eq,e1,e2,_), Skip, Revert, _)
    when is_address (get_type_exp e1) && is_address (get_type_exp e2)
         && (to_string_exp e1 = "msg.sender" || to_string_exp e2 = "msg.sender")
         -> []

  | If (BinOp (NEq,e1,e2,_), Revert, Skip, _)
    when is_address (get_type_exp e1) && is_address (get_type_exp e2)
         && (to_string_exp e1 = "msg.sender" || to_string_exp e2 = "msg.sender")
         -> []

  | If (exp, Skip, Revert, _) when is_safe_or exp -> []

  | If (BinOp(bop,e1,Lv (Var _),_), Skip, Revert, _)
    when to_string_exp ~report:true e1 = "this.balance" && bop = Gt || bop = GEq -> []

  | If (BinOp(bop,e1,Lv (Var _),_), Skip, Revert, _)
    when to_string_exp ~report:true e1 = "address(this).balance" && bop = Gt || bop = GEq -> []

  | If (e,s1,s2,i) ->
    let lst1 = exp_e i.if_loc e in
    let lst2 = collect_exp s1 in
    let lst3 = collect_exp s2 in
    lst1 @ lst2 @ lst3

    (* loop conditions are not considered from the search space *)
  | While (_e,s) -> (* (exp_e (-1) e) @ *) (collect_exp s)

  | Break | Continue -> []
  | Return (None,_) -> []
  | Return (Some e,loc) -> exp_e loc e
  | Revert -> []
  | Assume (e,loc) -> exp_e loc e
  | Assert (e,_,loc) -> exp_e loc e
  | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (blk,_) -> List.fold_left (fun acc s -> acc @ (collect_exp s)) [] blk

and exp_e : Loc.t -> exp -> (Loc.t * exp) list
= fun stmt_loc exp ->
  match exp with
  | V _ | Str _ -> []
  | Lv lv -> exp_lv stmt_loc lv
  | Cast (_typ,e) -> exp_e stmt_loc e
  | BinOp (_bop,_e1,_e2,einfo) when einfo.loc.line < 0 -> [] (* exclude exps generated during preprocessing *)
  | BinOp (_bop,e1,e2,_einfo) -> (stmt_loc, exp)::((exp_e stmt_loc e1) @ (exp_e stmt_loc e2))
  | UnOp (_uop,e,_typ) -> (stmt_loc, exp)::(exp_e stmt_loc e)
  | Ite (i, t, e, _) -> (stmt_loc, exp)::(exp_e stmt_loc i @ exp_e stmt_loc t @ exp_e stmt_loc e)
  | ETypeName _ -> []
  | IndexRangeAccess _ | TypeInfo _ -> []
  | IncTemp _ | DecTemp _ | CallTemp _
  | CondTemp _ | AssignTemp _ -> assert false

and exp_lv : Loc.t -> lv -> (Loc.t * exp) list
= fun stmt_loc lv ->
  match lv with
  | Var _ -> []
  | MemberAccess (e,_,_,_) -> exp_e stmt_loc e
  | IndexAccess (e1,Some e2,_) -> (exp_e stmt_loc e1) @ (exp_e stmt_loc e2)
  | IndexAccess (e,None,_) -> exp_e stmt_loc e
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | None -> []
      | Some e -> acc @ (exp_e stmt_loc e)
    ) [] eops
