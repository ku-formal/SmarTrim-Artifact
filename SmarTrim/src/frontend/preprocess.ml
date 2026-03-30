open Lang

(************************************)
(************************************)
(*** Move state_var_init to Cnstr ***)
(************************************)
(************************************)

let _decl_to_stmt (d : State_var_decl.t) : Stmt.t =
  match d.assign with
  | None -> Decl (Var (d.name, d.vinfo))
  | Some e ->
    if Typ.equal d.vinfo.vtyp (get_type_exp e) then Assign (Var (d.name, d.vinfo), e, d.vinfo.vloc)
    else Assign (Var (d.name, d.vinfo), Cast (d.vinfo.vtyp, e), d.vinfo.vloc)
;;

let decl_to_stmt (d : State_var_decl.t) : Stmt.t =
  let assign (d : State_var_decl.t) e = Stmt.Assign (Var (d.name, d.vinfo), e, d.vinfo.vloc) in
  match d.assign with
  | None -> begin
    match d.vinfo.vtyp with
    | EType (UInt _) | EType (SInt _) | EType Address | EType AddressPayable ->
      assign d (V (Int Z.zero))
    | EType Bool -> assign d (V (Bool false))
    | _ -> Decl (Var (d.name, d.vinfo))
  end
  | Some e -> assign d e
;;

let move_f decls func =
  if Func.is_constructor func then (* add initializations of decls to constructors *)
    let inits = List.fold_left (fun acc decl -> Stmt.Seq (acc, decl_to_stmt decl)) Skip decls in
    let body = Func.body func in
    let body' = Stmt.Seq (inits, body) in
    Field.fset Func.Fields.body func body'
  else func
;;

let move_c (c : Contract.t) = { c with funcs = List.map (move_f c.decls) c.funcs }
let move_p contracts = List.map move_c contracts
let move_decl_to_cnstr pgm = move_p pgm

(***********************)
(***********************)
(*** Replace TemExps ***)
(***********************)
(***********************)

let separator = "__@"

let rec hastmp_lv lv =
  match lv with
  | Var _ -> false
  | MemberAccess (e, _, _, _) -> hastmp_e e
  | IndexAccess (e, None, _) -> hastmp_e e
  | IndexAccess (e1, Some e2, _) -> hastmp_e e1 || hastmp_e e2
  | Tuple (eoplst, _) ->
    List.exists (fun eop -> match eop with None -> false | Some e -> hastmp_e e) eoplst

and hastmp_e e =
  match e with
  | V _ | Str _ -> false
  | Lv lv -> hastmp_lv lv
  | Cast (_, e) -> hastmp_e e
  | BinOp (_, e1, e2, _) -> hastmp_e e1 || hastmp_e e2
  | UnOp (_, e, _) -> hastmp_e e
  | Ite (i, t, e, _) -> hastmp_e i || hastmp_e t || hastmp_e e
  | ETypeName _ -> false
  | IndexRangeAccess (base, sop, fop, _) -> (
    hastmp_lv base
    || (match sop with None -> false | Some s -> hastmp_e s)
    || match fop with None -> false | Some f -> hastmp_e f)
  | TypeInfo _ -> false
  | AssignTemp _ | CondTemp _ | IncTemp _ | DecTemp _ | CallTemp _ -> true

and hastmp_s (s : Stmt.t) =
  match s with
  | Assign (lv, e, _) -> hastmp_lv lv || hastmp_e e
  | Decl _ -> false
  | Seq (s1, s2) -> hastmp_s s1 || hastmp_s s2
  | Call (lvop, e, args, ethop, gasop, _) ->
    let b1 = match lvop with None -> false | Some lv -> hastmp_lv lv in
    let b2 = hastmp_e e in
    let b3 = List.exists hastmp_e args in
    let b4 = match ethop with None -> false | Some e -> hastmp_e e in
    let b5 = match gasop with None -> false | Some e -> hastmp_e e in
    b1 || b2 || b3 || b4 || b5
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (id, fkey, is_static, loc);
    let b1 = match ret with None -> false | Some lv -> hastmp_lv lv in
    let b2 = hastmp_e target in
    let b3 = List.exists hastmp_e args in
    let b4 = match ether with None -> false | Some e -> hastmp_e e in
    let b5 = match gas with None -> false | Some e -> hastmp_e e in
    b1 || b2 || b3 || b4 || b5
  | Skip -> false
  | If (e, s1, s2, _) -> hastmp_e e || hastmp_s s1 || hastmp_s s2
  | While (e, s) -> hastmp_e e || hastmp_s s
  | Break -> false
  | Continue -> false
  | Return (None, _) -> false
  | Return (Some e, _) -> hastmp_e e
  | Revert -> false
  | Assume (e, _) -> hastmp_e e
  | Assert (e, _, _) -> hastmp_e e
  | Assembly _ | Placeholder -> false
  | Unchecked (lst, _loc) -> List.exists hastmp_s lst
  | Label _ -> false
;;

let hastmp_f f = hastmp_s f.Func.body
let hastmp_c (c : Contract.t) = List.exists hastmp_f c.funcs
let hastmp_p contracts = List.exists hastmp_c contracts
let hastmp p = hastmp_p p

(* Given a exp, returns a pair of (replaced exp,new stmt) *)
let rec replace_tmpexp_e (exp : exp) : exp * Stmt.t =
  match exp with
  | V _ | Str _ -> (exp, Skip)
  | Lv lv ->
    let lv', s = replace_tmpexp_lv lv in
    (Lv lv', s)
  | Cast (typ, e) ->
    let e', s = replace_tmpexp_e e in
    (Cast (typ, e'), s)
  | BinOp (bop, e1, e2, einfo) ->
    let e1', s1 = replace_tmpexp_e e1 in
    let e2', s2 = replace_tmpexp_e e2 in
    (BinOp (bop, e1', e2', einfo), Seq (s1, s2))
  | UnOp (uop, e, typ) ->
    let e', s = replace_tmpexp_e e in
    (UnOp (uop, e', typ), s)
  | ETypeName _ -> (exp, Skip)
  | IndexRangeAccess (base, sop, fop, einfo) ->
    let base', s1 = replace_tmpexp_lv base in
    let sop', s2 =
      match sop with
      | Some start ->
        let e, s = replace_tmpexp_e start in
        (Some e, s)
      | None -> (None, Skip)
    in
    let fop', s3 =
      match fop with
      | Some fin ->
        let e, s = replace_tmpexp_e fin in
        (Some e, s)
      | None -> (None, Skip)
    in
    (IndexRangeAccess (base', sop', fop', einfo), Seq (s1, Seq (s2, s3)))
  | TypeInfo _ -> (exp, Skip)
  | IncTemp (Lv lv, prefix, loc) when prefix ->
    let typ = get_type_lv lv in
    (Lv lv, Assign (lv, BinOp (Add, Lv lv, V (Int Z.one), { loc; typ; id = 0 }), loc))
  | IncTemp (Lv lv, _, loc) ->
    (* postfix case *)
    let typ = get_type_lv lv in
    let tmpvar = gen_tmpvar typ in
    ( Lv tmpvar,
      Seq
        ( Assign (tmpvar, Lv lv, loc),
          Assign (lv, BinOp (Add, Lv lv, V (Int Z.one), { loc; typ; id = 0 }), loc) ) )
  | DecTemp (Lv lv, prefix, loc) when prefix ->
    let typ = get_type_lv lv in
    (Lv lv, Assign (lv, BinOp (Sub, Lv lv, V (Int Z.one), { loc; typ; id = 0 }), loc))
  | DecTemp (Lv lv, _, loc) ->
    (* postfix case *)
    let typ = get_type_lv lv in
    let tmpvar = gen_tmpvar typ in
    ( Lv tmpvar,
      Seq
        ( Assign (tmpvar, Lv lv, loc),
          Assign (lv, BinOp (Sub, Lv lv, V (Int Z.one), { loc; typ; id = 0 }), loc) ) )
  | CallTemp (Lv (MemberAccess (Cast (t, e), id, id_info, typ)), params, ethop, gasop, einfo) ->
    (* ... := cast(y).f(33) *)
    let tmpvar = gen_tmpvar ~org:(Some (Cast (t, e))) t in
    let exp' =
      CallTemp (Lv (MemberAccess (Lv tmpvar, id, id_info, typ)), params, ethop, gasop, einfo)
    in
    let new_stmt = Stmt.Assign (tmpvar, Cast (t, e), einfo.loc) in
    (exp', new_stmt)
  | CallTemp (e, params, ethop, gasop, einfo) ->
    (* NOTE: currently, assume function variable's type is equal to its return type. *)
    if Typ.is_tuple einfo.typ then
      let tmpvars =
        List.map (gen_tmpvar ~org:(Some exp) ~loc:einfo.loc.line) (Typ.tuple_elem einfo.typ)
      in
      let eoplst = List.map (fun tmp -> Some (Lv tmp)) tmpvars in
      let tuple = Tuple (eoplst, einfo.typ) in
      (Lv tuple, Call (Some tuple, e, params, ethop, gasop, einfo.loc))
    else
      let tmpvar = gen_tmpvar ~org:(Some exp) ~loc:einfo.loc.line einfo.typ in
      (Lv tmpvar, Call (Some tmpvar, e, params, ethop, gasop, einfo.loc))
  | CondTemp (e1, e2, e3, typ, loc) -> (
    match (e2, e3) with
    | Lv (Tuple (_eops1, t1)), Lv (Tuple (_eops2, t2)) ->
      let _ = assert (t1 = t2) in
      let tmpvars = List.map (gen_tmpvar ~org:(Some exp)) (Typ.tuple_elem t1) in
      let tuple = Tuple (List.map (fun tmp -> Some (Lv tmp)) tmpvars, t1) in
      ( Lv tuple,
        Seq (Decl tuple, If (e1, Assign (tuple, e2, loc), Assign (tuple, e3, loc), Ifinfo.dummy)) )
    | Lv (Tuple _), _ | _, Lv (Tuple _) -> assert false
    | _ ->
      let tmpvar = gen_tmpvar ~org:(Some exp) typ in
      ( Lv tmpvar,
        Seq (Decl tmpvar, If (e1, Assign (tmpvar, e2, loc), Assign (tmpvar, e3, loc), Ifinfo.dummy))
      ))
  | AssignTemp (lv, e, loc) -> (Lv lv, Assign (lv, e, loc))
  | e -> raise (Failure ("replace_tmpexp_e : " ^ to_string_exp e))

and replace_tmpexp_lv : lv -> lv * Stmt.t =
 fun lv ->
  match lv with
  | Var _ -> (lv, Skip)
  | MemberAccess (Cast (t, e), id, id_info, typ) ->
    let tmpvar = gen_tmpvar ~org:(Some (Cast (t, e))) t in
    (MemberAccess (Lv tmpvar, id, id_info, typ), Assign (tmpvar, Cast (t, e), id_info.vloc))
  | MemberAccess (e, id, id_info, typ) ->
    let e', s = replace_tmpexp_e e in
    (MemberAccess (e', id, id_info, typ), s)
  | IndexAccess (e, None, typ) ->
    let e', s = replace_tmpexp_e e in
    (IndexAccess (e', None, typ), s)
  | IndexAccess (e1, Some e2, typ) ->
    let e1', s1 = replace_tmpexp_e e1 in
    let e2', s2 = replace_tmpexp_e e2 in
    (IndexAccess (e1', Some e2', typ), Seq (s1, s2))
  | Tuple (eoplst, typ) ->
    let eoplst', final_s =
      List.fold_left
        (fun (acc_lst, acc_s) eop ->
          match eop with
          | None -> (acc_lst @ [ None ], acc_s)
          | Some e ->
            let e', s = replace_tmpexp_e e in
            (acc_lst @ [ Some e' ], Stmt.Seq (acc_s, s)))
        ([], Skip) eoplst
    in
    (Tuple (eoplst', typ), final_s)
;;

let replace_tmpexp_lvop (lvop : lv option) : lv option * Stmt.t =
  match lvop with
  | None -> (None, Skip)
  | Some lv ->
    let lv', stmt = replace_tmpexp_lv lv in
    (Some lv', stmt)
;;

let replace_tmpexp_eop (eop : exp option) : exp option * Stmt.t =
  match eop with
  | None -> (None, Skip)
  | Some e ->
    let e', stmt = replace_tmpexp_e e in
    (Some e', stmt)
;;

let has_value_gas_modifiers_old_solc exp =
  match exp with
  | CallTemp (Lv (MemberAccess (_e, "gas", _, _)), _, None, None, _) -> true
  | CallTemp (Lv (MemberAccess (_e, "value", _, _)), _, None, None, _) -> true
  | CallTemp (Lv (MemberAccess (_e, "gas", _, _)), _, _, _, _) -> assert false
  | CallTemp (Lv (MemberAccess (_e, "value", _, _)), _, _, _, _) -> assert false
  | _ -> false
;;

(* e.g., given f.gas(10).value(5).gas(3) , return f *)
let rec remove_value_gas_modifiers exp =
  match exp with
  | CallTemp (Lv (MemberAccess (e, "gas", _, _)), _, _, _, _) ->
    remove_value_gas_modifiers e (* remove gas modifier chains, e.g., e.gas(10)(arg) => e(arg) *)
  | CallTemp (Lv (MemberAccess (e, "value", _, _)), _, _, _, _) ->
    remove_value_gas_modifiers e (* remove value modifier chains *)
  | _ -> exp
;;

(* get outer-most argument of gas modifier *)
let rec get_gasop exp =
  match exp with
  (* | Lv (MemberAccess (e,"call",_,_)) when is_address (get_type_exp e) -> Int BatBig_int.zero *)
  | CallTemp (Lv (MemberAccess (_e, "gas", _, _)), args, _, _, _) ->
    let _ = assert (List.length args = 1) in
    Some (List.hd args)
  | CallTemp (Lv (MemberAccess (e, "value", _, _)), _, _, _, _) -> get_gasop e
  | _ -> None
;;

(* get outer-most argument of value modifier *)
let rec get_valueop exp =
  match exp with
  | CallTemp (Lv (MemberAccess (e, "gas", _, _)), _, _, _, _) -> get_valueop e
  | CallTemp (Lv (MemberAccess (_e, "value", _, _)), args, _, _, _) ->
    let _ = assert (List.length args = 1) in
    Some (List.hd args)
  | _ -> None
;;

let desugar_tuple (lv, e, loc) : Stmt.t =
  match (lv, e) with
  | Tuple (eops1, _), Lv (Tuple (eops2, _)) ->
    List.fold_left2
      (fun acc eop1 eop2 ->
        match (eop1, eop2) with
        | Some (Lv lv'), Some e' -> Stmt.Seq (acc, Assign (lv', e', loc))
        | None, Some _e' -> acc
        | _ -> assert false)
      Skip eops1 eops2
  | _ -> Assign (lv, e, loc)
;;

let rec replace_tmpexp_s (stmt : Stmt.t) : Stmt.t =
  match stmt with
  (* E.g., (bool success, ) := msg.sender.call.value(..)(..) *)
  | Assign (lv, CallTemp (e, params, ethop, gasop, _einfo), loc) ->
    Call (Some lv, e, params, ethop, gasop, loc)
  | Assign (lv, e, loc) ->
    let lv', new_stmt1 = replace_tmpexp_lv lv in
    let e', new_stmt2 = replace_tmpexp_e e in
    let assigns = desugar_tuple (lv', e', loc) in
    Seq (Seq (new_stmt1, new_stmt2), assigns)
  | Decl _lv -> stmt
  | Seq (s1, s2) -> Seq (replace_tmpexp_s s1, replace_tmpexp_s s2)
  | Call (lvop, e, params, _, _, loc) when has_value_gas_modifiers_old_solc e ->
    assert (no_eth_gas_modifiers stmt);
    (* ethop = gasop = None *)
    let ethop = get_valueop e in
    let gasop = get_gasop e in
    let e' = remove_value_gas_modifiers e in
    let lvop', s1 = replace_tmpexp_lvop lvop in
    let e'', s2 = replace_tmpexp_e e' in
    Seq (Seq (s1, s2), Call (lvop', e'', params, ethop, gasop, loc))
  | Call (lvop, e, params, ethop, gasop, loc) ->
    let lvop', s1 = replace_tmpexp_lvop lvop in
    let e', s2 = replace_tmpexp_e e in
    let params', s3 =
      List.fold_left
        (fun (acc_params, acc_stmt) param ->
          let param', s = replace_tmpexp_e param in
          (acc_params @ [ param' ], Stmt.Seq (acc_stmt, s)))
        ([], Skip) params
    in
    let ethop', s4 = replace_tmpexp_eop ethop in
    let gasop', s5 = replace_tmpexp_eop gasop in
    Seq (s1, Seq (s2, Seq (s3, Seq (s4, Seq (s5, Call (lvop', e', params', ethop', gasop', loc))))))
  | Extcall _ ->
    Pp.invalid_argf "preprocess.ml : Extcall constructors appear only in preprocess phase 2"
  | Skip -> stmt
  | If (e, s1, s2, i) ->
    let e', new_stmt = replace_tmpexp_e e in
    Seq (new_stmt, If (e', replace_tmpexp_s s1, replace_tmpexp_s s2, i))
  | While (e, s) ->
    let e', new_stmt = replace_tmpexp_e e in
    Seq (new_stmt, While (e', Seq (replace_tmpexp_s s, new_stmt)))
  | Break -> stmt
  | Continue -> stmt
  | Return (None, _) -> stmt
  | Return (Some (CallTemp (e, params, ethop, gasop, einfo)), loc) when einfo.typ = TupleType [] ->
    (* e.g., examples/compilable.sol *)
    let s1 = Stmt.Call (None, e, params, ethop, gasop, loc) in
    let s2 = Stmt.Return (None, loc) in
    Seq (s1, s2)
  | Return (Some e, loc_ret) -> (
    let e', new_stmt = replace_tmpexp_e e in
    match (e', new_stmt) with
    | Lv (Tuple ([], _)), Call (Some (Tuple ([], _)), e, args, ethop, gasop, loc) ->
      (* "return f()"; where f() returns void. *)
      Seq (Call (None, e, args, ethop, gasop, loc), Return (None, loc_ret))
    | _ -> Seq (new_stmt, Return (Some e', loc_ret)))
  | Revert -> stmt
  | Assume (e, loc) ->
    let e', new_stmt = replace_tmpexp_e e in
    Seq (new_stmt, Assume (e', loc))
  | Assert (e, vtyp, loc) ->
    let e', new_stmt = replace_tmpexp_e e in
    Seq (new_stmt, Assert (e', vtyp, loc))
  | Assembly _ | Placeholder | Label _ -> stmt
  | Unchecked (lst, loc) ->
    let lst' = List.map replace_tmpexp_s lst in
    Unchecked (lst', loc)
;;

let replace_tmpexp_f (f : Func.t) = { f with body = replace_tmpexp_s f.body }

let replace_tmpexp_c : Contract.t -> Contract.t =
 fun c -> { c with funcs = List.map replace_tmpexp_f c.funcs }
;;

let replace_tmpexp_p (pgm : Pgm.t) : Pgm.t = List.map replace_tmpexp_c pgm

let rec loop f pgm =
  let pgm' = f pgm in
  if not (hastmp pgm') then pgm' else loop f pgm'
;;

let replace_tmpexp (pgm : Pgm.t) : Pgm.t = loop replace_tmpexp_p pgm

(******************)
(******************)
(** Remove Skips **)
(******************)
(******************)

let rec rmskip_s (s : Stmt.t) : Stmt.t =
  match s with
  | Seq (Skip, s) -> rmskip_s s
  | Seq (s, Skip) -> rmskip_s s
  | Seq (s1, s2) -> Seq (rmskip_s s1, rmskip_s s2)
  | If (e, s1, s2, i) -> If (e, rmskip_s s1, rmskip_s s2, i)
  | While (e, s) -> While (e, rmskip_s s)
  | Unchecked (lst, loc) -> Unchecked (List.map rmskip_s lst, loc)
  | _ -> s
;;

let rmskip_f (f : Func.t) = { f with body = rmskip_s f.body }
let rmskip_c (c : Contract.t) = { c with funcs = List.map rmskip_f c.funcs }
let rmskip_p contracts = List.map rmskip_c contracts
let rmskip p = p |> rmskip_p |> rmskip_p |> rmskip_p

(*******************************)
(*******************************)
(** Normalize many variations **)
(*******************************)
(*******************************)

let rec norm_s ret_params (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Seq (s1, s2) -> Seq (norm_s ret_params s1, norm_s ret_params s2)
  | If (e, s1, s2, i) -> If (e, norm_s ret_params s1, norm_s ret_params s2, i)
  | While (e, s) -> While (e, norm_s ret_params s)
  | Call
      ( lvop,
        Lv (MemberAccess ((Lv (IndexAccess _) as arr), fname, fname_info, typ)),
        exps,
        ethop,
        gasop,
        loc ) ->
    let tmp = gen_tmpvar ~org:(Some arr) (get_type_exp arr) in
    let assign = Stmt.Assign (tmp, arr, loc) in
    let e' = Lv (MemberAccess (Lv tmp, fname, fname_info, typ)) in
    let call = Stmt.Call (lvop, e', exps, ethop, gasop, loc) in
    Seq (assign, call)
  | Return (None, _loc) -> stmt
  | Return (Some (Lv (Tuple ([], _))), loc) -> Return (None, loc) (* return (); => return; *)
  | Return (Some (Lv (Var _)), _loc) -> stmt
  | Return (Some e, loc) ->
    let lv = params_to_lv ret_params in
    let assign = Stmt.Assign (lv, e, loc) in
    let ret_stmt = Stmt.Return (Some (Lv lv), loc) in
    let stmt' = Stmt.Seq (assign, ret_stmt) in
    stmt'
  | _ -> stmt
;;

let norm_f func =
  let ret = func.Func.ret_params in
  let stmt = Func.body func in
  let stmt' = norm_s ret stmt in
  Field.fset Func.Fields.body func stmt'
;;

let norm_c (c : Contract.t) = { c with funcs = List.map norm_f c.funcs }
let norm_p contracts = List.map norm_c contracts
let normalize p = norm_p p

(***********************************)
(***********************************)
(** Handling Using-for-Directives **)
(***********************************)
(***********************************)

let find_matching_lib_name lib_funcs callee_name arg_typs =
  let matching_func =
    List.find
      (fun f ->
        let param_typs = Func.param_types f in
        String.equal f.Func.name callee_name
        && List.length arg_typs = List.length param_typs
        &&
        (* should be checked first before checking convertibility *)
        List.for_all2 Typ.is_implicitly_convertible arg_typs param_typs)
      lib_funcs
  in
  matching_func.Func.info.scope_s
;;

let rec ufd_s (lst : (string * Directive.t) list) (lib_funcs : Func.t list) (stmt : Stmt.t) : Stmt.t
    =
  let lib_names = List.map fst lst in
  match stmt with
  | Call (lvop, Lv (MemberAccess (e, fname, fname_info, typ)), args, ethop, gasop, loc)
    when List.mem fname (List.map (fun f -> f.Func.name) lib_funcs)
         (* e.g., (a+b).add(c) when using SafeMath *)
         && not (List.mem (to_string_exp e) lib_names)
         (* e.g., SafeMath.add (a,b) should not be changed. *) -> (
    let e_typ = get_type_exp e in
    let lst' = List.filter Directive.(fun (_, t) -> t = Typ e_typ || t = Glob) lst in
    let cand_lib_names = List.map fst lst' in
    match List.length cand_lib_names with
    | 0 -> stmt (* no using for syntax *)
    | _ ->
      let arg_typs = List.map get_type_exp (e :: args) in
      (* move the receiver to the first argument *)
      let lib_funcs' =
        List.filter (fun f -> List.mem f.Func.info.scope_s cand_lib_names) lib_funcs
      in
      let lib_name = find_matching_lib_name lib_funcs' fname arg_typs in
      (* from libs, using fname and arg_typs, find corresponding library name *)
      let lib_typ = Typ.EType (Contract lib_name) in
      let lib_var = Lv (Var (lib_name, mk_vinfo ~typ:lib_typ ())) in
      Call (lvop, Lv (MemberAccess (lib_var, fname, fname_info, typ)), e :: args, ethop, gasop, loc)
    )
  | Extcall _ ->
    Pp.invalid_argf "preprocess.ml : Extcall constructors appear only in preprocess phase 2"
  | Seq (s1, s2) -> Seq (ufd_s lst lib_funcs s1, ufd_s lst lib_funcs s2)
  | If (e, s1, s2, i) -> If (e, ufd_s lst lib_funcs s1, ufd_s lst lib_funcs s2, i)
  | While (e, s) -> While (e, ufd_s lst lib_funcs s)
  | Call _ | Assign _ | Decl _ | Skip | Break | Continue | Return _ | Revert | Assume _ | Assert _
  | Assembly _ | Placeholder | Label _ ->
    stmt
  | Unchecked (blk, loc) ->
    let blk' = List.map (ufd_s lst lib_funcs) blk in
    Unchecked (blk', loc)
;;

let ufd_f lst lib_funcs f = Func.{ f with body = ufd_s lst lib_funcs f.body }

let ufd_c pgm (c : Contract.t) =
  let lib_names = List.map fst c.info.lib_typ_list in
  let libs = List.map (Pgm.find_by_name pgm) lib_names in
  let lib_funcs = List.fold_left (fun acc (lib : Contract.t) -> acc @ lib.funcs) [] libs in
  Field.map Contract.Fields.funcs ~f:(List.map (ufd_f c.info.lib_typ_list lib_funcs)) c
;;

let ufd_p contracts = List.map (ufd_c contracts) contracts
let ufd p = ufd_p p (* abbreviation for using for directives *)

let prop_libs_c : Contract.t list -> Contract.t -> Contract.t =
 fun parents c ->
  (* propagate parents => c *)
  List.fold_left
    (fun (acc : Contract.t) (parent : Contract.t) ->
      let libs_parent = parent.info.lib_typ_list in
      let acc_info = acc.info in
      let libs' = Set.(to_list (union (of_list libs_parent) (of_list acc_info.lib_typ_list))) in
      Field.map Contract.Fields.info ~f:(const { acc_info with lib_typ_list = libs' }) acc)
    c parents
;;

let prop_libs_p p =
  List.map
    (fun c ->
      let nids_of_parents = Contract.inherit_order c in
      let parents = List.tl (List.map (Pgm.find_nid p) nids_of_parents) in
      prop_libs_c parents c)
    p
;;

let propagate_libtyp pgm = prop_libs_p pgm
let replace_lib_calls pgm = pgm |> propagate_libtyp |> ufd

(**************************************)
(****** Add contract/lib name to ******)
(** function calls without prefixes ***)
(**************************************)

let rec add_cname_s : string -> Var.t list -> Stmt.t -> Stmt.t =
 fun cname func_typ_params stmt ->
  match stmt with
  | Seq (s1, s2) -> Seq (add_cname_s cname func_typ_params s1, add_cname_s cname func_typ_params s2)
  | If (e, s1, s2, i) ->
    If (e, add_cname_s cname func_typ_params s1, add_cname_s cname func_typ_params s2, i)
  | Call (lvop, Lv (Var (v, vinfo)), args, ethop, gasop, loc)
    when (not (List.mem v built_in_funcs))
         && (not (List.mem v init_funcs))
         && not (List.mem (v, vinfo.vtyp) func_typ_params)
         (* function pointer *) ->
    let prefix = Lv (Var (cname, mk_vinfo ~typ:(EType (Contract cname)) ())) in
    Call (lvop, Lv (MemberAccess (prefix, v, vinfo, vinfo.vtyp)), args, ethop, gasop, loc)
  | While (e, s) -> While (e, add_cname_s cname func_typ_params s)
  | Unchecked (lst, loc) ->
    let lst' = List.map (add_cname_s cname func_typ_params) lst in
    Unchecked (lst', loc)
  | _ -> stmt
;;

let add_cname_f cname (f : Func.t) =
  let func_typ_params = List.filter (fun v -> Typ.is_func (snd v)) (Func.param_vars f) in
  let old_stmt = Func.body f in
  let new_stmt = add_cname_s cname func_typ_params old_stmt in
  Field.fset Func.Fields.body f new_stmt
;;

let add_cname_c c =
  let cname = Contract.name c in
  let old_funcs = Contract.funcs c in
  let new_funcs = List.map (add_cname_f cname) old_funcs in
  { c with funcs = new_funcs }
;;

let add_cname_p contracts = List.map add_cname_c contracts
let add_cname_fcalls p = add_cname_p p

(*****************************)
(*****************************)
(** Merge into one contract **)
(*****************************)
(*****************************)

let find_next_contracts : Contract.t list -> string -> Contract.t list =
 fun lst target ->
  let names = List.map Contract.name lst in
  let target_idx = match List.index_of target names with Some idx -> idx | None -> assert false in
  List.fold_lefti (fun acc i c -> if i < target_idx + 1 then acc else acc @ [ c ]) [] lst
;;

let add_func : Func.t -> Contract.t -> Contract.t =
 fun f c ->
  let b = List.exists (Func.equal_sig f) c.funcs || f.Func.info.is_constructor in
  (* Do not copy *)
  (* 1. if functions are constructors, and  *)
  (* 2. if functions with the same signatures are already exist in the contract *)
  if b then c
  else
    let old_finfo = f.Func.info in
    let new_finfo = { old_finfo with scope = c.info.numid; scope_s = c.name } in
    let new_f = Field.fset Func.Fields.info f new_finfo in
    { c with funcs = c.funcs @ [ new_f ] }
;;

let add_func2 : Contract.t -> Contract.t -> Contract.t =
 fun from_ to_ ->
  let funcs = from_.funcs in
  List.fold_left (fun acc f -> add_func f acc) to_ funcs
;;

let add_decl : State_var_decl.t -> Contract.t -> Contract.t =
 fun cand contract -> { contract with decls = contract.decls @ [ cand ] }
;;

let add_decl2 : Contract.t -> Contract.t -> Contract.t =
 fun from_ to_ ->
  let decls = from_.decls in
  List.fold_left (fun acc d -> add_decl d acc) to_ decls
;;

let add_enum : Contract.t -> Contract.t -> Contract.t =
 fun from_ to_ ->
  (* Duplicated (i.e., already declared) enums by inheritance will be rejected by solc, so just copy enums. *)
  let enums1 = from_.enums in
  let enums2 = to_.enums in
  { to_ with enums = enums1 @ enums2 }
;;

let add_struct : Contract.t -> Contract.t -> Contract.t =
 fun from_ to_ ->
  (* Similarly, duplicated (i.e., already declared) structures by inheritance will be rejected by solc, so just copy structures. *)
  let structs1 = from_.structs in
  let structs2 = to_.structs in
  { to_ with structs = structs1 @ structs2 }
;;

let add_cnstr_mod_call' (from_ : Func.t) (to_ : Func.t) : Func.t =
  assert (Func.(is_constructor from_ && is_constructor to_));
  let modcall_from = List.rev from_.Func.info.mod_list2 in
  let modcall_to = to_.Func.info.mod_list2 in
  (* duplicated consturctor modifier invocation is error in solc >= 0.5.0,
   * but perform deduplication for compatibility with solc <= 0.4.26 *)
  let modcall_to' =
    List.fold_left
      (fun acc (m : Mod_call.t) ->
        let b = List.exists (fun (m' : Mod_call.t) -> m.id = m'.id) acc in
        if b then acc else m :: acc)
      modcall_to modcall_from
  in
  let finfo_to = to_.info in
  let finfo_to = { finfo_to with mod_list2 = modcall_to' } in
  let finfo_to = Field.fset Func.Fields.info to_ finfo_to in
  finfo_to
;;

let add_cnstr_mod_call (from_ : Contract.t) (to_ : Contract.t) : Contract.t =
  let funcs = to_.funcs in
  let funcs' =
    List.map
      (fun f ->
        if Func.is_constructor f then
          add_cnstr_mod_call' (Contract.cnstr from_) (Contract.cnstr to_)
        else f)
      funcs
  in
  { to_ with funcs = funcs' }
;;

let debug_abstract_contract parent c =
  if
    Contract.name c = !Options.main_contract
    && parent |> Contract.cnstr |> Func.params |> List.length <> 0
  then (
    prerr_endline ("[WARNING] contract " ^ Contract.name c ^ " may be abstract");
    prerr_endline
      ("- arguments for base contract " ^ Contract.name parent ^ " constructor are not provided\n"))
;;

let copy_c : Contract.t list -> Contract.t -> Contract.t =
 fun parents c ->
  let c' =
    List.fold_left
      (fun acc parent ->
        acc |> add_func2 parent |> add_decl2 parent |> add_enum parent |> add_struct parent
        |> add_cnstr_mod_call parent)
      c parents
  in
  (* reorder constructor modifier invocations according to inheritance order *)
  let funcs = c'.funcs in
  let funcs' =
    List.map
      (fun f ->
        if Func.is_constructor f then
          let finfo = f.Func.info in
          let cnstr_mod_calls = finfo.mod_list2 in
          let cnstr_mod_calls' =
            (* recursive constructor calls are removed, as we iterate over parents. e.g., contract A { constructor (uint n) A (5) ... } *)
            List.fold_left
              (fun acc parent ->
                let matching =
                  List.filter (fun (m : Mod_call.t) -> Contract.name parent = m.id) cnstr_mod_calls
                in
                let _ = assert (List.length matching = 1 || List.length matching = 0) in
                if List.length matching = 1 then acc @ [ List.hd matching ]
                else
                  let _ = debug_abstract_contract parent c in
                  acc @ [ { id = parent.name; args = []; loc = Loc.dummy } ])
              [] (List.rev parents)
          in
          (* reverse to put parent's mod on the front. *)
          let finfo = { finfo with mod_list2 = cnstr_mod_calls' } in
          let f = Field.fset Func.Fields.info f finfo in
          f
        else f)
      funcs
  in
  { c' with funcs = funcs' }
;;

let get_full_base_contracts pgm c =
  let bases = c.Contract.info.inherit_order in
  (* left: main (child), right: parent *)
  let bases = List.map (Pgm.find_nid pgm) bases in
  bases
;;

let copy_p : Pgm.t -> Pgm.t =
 fun p ->
  List.map
    (fun c ->
      let parents = List.tl (get_full_base_contracts p c) in
      copy_c parents c)
    p
;;

let copy pgm = copy_p pgm

(*********************)
(*********************)
(** Replace 'super' **)
(*********************)
(*********************)

let rec rs_s (family : Contract.t list) (cur_cname : string) (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Assign _ -> stmt
  | Decl _ -> stmt
  | Seq (s1, s2) -> Seq (rs_s family cur_cname s1, rs_s family cur_cname s2)
  | Call (lvop, Lv (MemberAccess (Lv (Var (x, xinfo)), id, id_info, typ)), args, ethop, gasop, loc)
    when String.equal x "super" ->
    let arg_typs = List.map get_type_exp args in
    let supers = find_next_contracts family cur_cname in
    let matched_parent =
      List.find
        (fun super ->
          let funcs = super.Contract.funcs in
          List.exists
            (fun f ->
              let id', typs' = Func.fsig f in
              if String.equal id id' && List.length arg_typs = List.length typs' then
                List.for_all2 Typ.is_implicitly_convertible arg_typs typs'
              else false)
            funcs)
        supers
    in
    let matched_parent_name = matched_parent.name in
    Call
      ( lvop,
        Lv (MemberAccess (Lv (Var (matched_parent_name, xinfo)), id, id_info, typ)),
        args,
        ethop,
        gasop,
        loc )
  | Call _ -> stmt
  | Skip -> stmt
  | If (e, s1, s2, i) -> If (e, rs_s family cur_cname s1, rs_s family cur_cname s2, i)
  | While (e, s) -> While (e, rs_s family cur_cname s)
  | _ -> stmt
;;

let rs_f : Contract.t list -> string -> Func.t -> Func.t =
 fun final_inherit cur_cname f ->
  let old_body = Func.body f in
  let new_body = rs_s final_inherit cur_cname old_body in
  Field.fset Func.Fields.body f new_body
;;

let rs_c : Contract.t list -> Contract.t -> Contract.t =
 fun final_inherit c ->
  let cur_cname = c.name in
  let old_funcs = c.funcs in
  let new_funcs = List.map (rs_f final_inherit cur_cname) old_funcs in
  { c with funcs = new_funcs }
;;

let rs_p (p : Pgm.t) : Pgm.t =
  let main = Pgm.main p in
  let nids_of_parents = Contract.inherit_order main in
  let final_inherit = List.map (Pgm.find_nid p) nids_of_parents in
  let family_names = List.map Contract.name final_inherit in
  List.fold_left
    (fun acc c ->
      (* NOTE: If a contract is not in the final inheritance graph,
       * NOTE: that means the super keywords in the contract need not be changed,
       * NOTE: because the contract is irrelevant with the main contract,
       * NOTE: i.e., the contract is dead code where there are no functions to be copied to the main contract.
       * NOTE: Therefore, we skip those contracts without modifications, i.e., 'acc' in the 'else' case.
       * NOTE: Without checking this condition, we may encounter exceptions,
       * NOTE: since we cannot find next contracts, see 'find_next_contracts'. *)
      if List.mem c.Contract.name family_names then
        let c' = rs_c final_inherit c in
        Pgm.modify c' acc
      else acc)
    p p
;;

let replace_super pgm = rs_p pgm

(**********************)
(**********************)
(** Generate getters **)
(**********************)
(**********************)

let get_public_state_vars (c : Contract.t) : (string * vinfo) list =
  let open State_var_decl in
  let decls = c.decls in
  let decls' = List.filter (fun d -> d.vinfo.vvis = Public) decls in
  List.map (fun d -> (d.name, d.vinfo)) decls'
;;

let code_public_getter = -10

(** generate getter functions for public state variables.

    TODO: currently does not deal with variables whose retval is struct type. *)
let add_getter_x (cname : string) (cnumid : int) ((x, xinfo) : string * vinfo) : Func.t option =
  let param_typs, ret_typ = Typ.domain_range_chain xinfo.vtyp in
  if Typ.is_struct ret_typ then None
  else
    let gen_param typ : string * vinfo = (Translator.gen_param_name (), mk_vinfo ~typ ()) in
    let params = List.map gen_param param_typs in
    let param_exps : exp list = List.map (fun v -> Lv (Var v)) params in
    let ret = (Translator.gen_param_name (), mk_vinfo ~typ:ret_typ ()) in
    let rec mk_e (e : exp) (args : exp list) =
      match args with
      | [] -> e
      | h :: t ->
        let e = mk_index_access e h in
        mk_e e t
    in
    let e = mk_e (Lv (Var (x, xinfo))) param_exps in
    let stmt = Stmt.Return (Some e, Loc.mk ~line:code_public_getter ()) in
    let info : Finfo.t =
      {
        is_constructor = false;
        is_payable = false;
        is_modifier = false;
        mod_list = [];
        mod_list2 = [];
        param_loc = Loc.dummy;
        ret_param_loc = Loc.dummy;
        fvis = External;
        mutability = View;
        fid = -1;
        floc = Loc.dummy;
        blk_loc = Loc.dummy;
        scope = cnumid;
        scope_s = cname;
        org_scope_s = cname;
        cfg = Cfg.empty;
      }
    in
    let func = Func.mk ~name:x ~params ~ret_params:[ ret ] ~body:stmt ~info in
    Some func
;;

let add_getter_c (c : Contract.t) : Contract.t =
  let cname = c.name in
  let cnumid = c.info.numid in
  let vars = get_public_state_vars c in
  let folder acc x =
    let f = add_getter_x cname cnumid x in
    match f with Some f -> add_func f acc | None -> acc
  in
  List.fold_left folder c vars
;;

let add_getter p =
  List.fold_left
    (fun acc c ->
      let c' = add_getter_c c in
      let acc' = Pgm.modify c' acc in
      acc')
    p p
;;

let bind_params : Loc.t -> (string * vinfo) list -> exp list -> Stmt.t =
 fun loc params args ->
  try
    List.fold_left2
      (fun acc (x, xinfo) arg -> Stmt.Seq (acc, Assign (Var (x, xinfo), arg, loc)))
      Skip params args
  with Invalid_argument _ -> Skip
;;

let rec replace_ph (mod_body : Stmt.t) (body : Stmt.t) : Stmt.t =
  match mod_body with
  | Placeholder -> body
  | Seq (s1, s2) -> Seq (replace_ph s1 body, replace_ph s2 body)
  | While (e, s) -> While (e, replace_ph s body)
  | If (e, s1, s2, i) -> If (e, replace_ph s1 body, replace_ph s2 body, i)
  | _ -> mod_body
;;

let rec has_ph (stmt : Stmt.t) : bool =
  match stmt with
  | Placeholder -> true
  | Seq (s1, s2) -> has_ph s1 || has_ph s2
  | While (_, s) -> has_ph s
  | If (_, s1, s2, _) -> has_ph s1 || has_ph s2
  | _ -> false
;;

let assign_after_ph (stmt : Stmt.t) : bool =
  match stmt with Seq (Seq (s, Placeholder), Assign _) when not (has_ph s) -> true | _ -> false
;;

let split_mod (stmt : Stmt.t) : Stmt.t * Stmt.t =
  match stmt with
  | Seq ((Seq (s, Placeholder) as s1), (Assign _ as s2)) when not (has_ph s) -> (s1, s2)
  | _ -> assert false
;;

let rec insert (stmt : Stmt.t) (mod_post : Stmt.t) : Stmt.t =
  match stmt with
  | Seq (s1, s2) -> Seq (insert s1 mod_post, insert s2 mod_post)
  | While (e, s) -> While (e, insert s mod_post)
  | If (e, s1, s2, i) -> If (e, insert s1 mod_post, insert s2 mod_post, i)
  | Return _ -> Seq (mod_post, stmt)
  | _ -> stmt
;;

let inline_mod_calls_f : Func.t list -> Func.t -> Func.t =
 fun funcs f ->
  let body = Func.body f in
  let mods = List.rev f.Func.info.mod_list in
  let body' =
    List.fold_left
      (fun acc (m : Mod_call.t) ->
        let mod_def = List.find (fun f -> f.Func.name = m.id) funcs in
        let binding = bind_params m.loc mod_def.params m.args in
        let mod_body = Func.body mod_def in
        if not (assign_after_ph mod_body) then Stmt.Seq (binding, replace_ph mod_body acc)
        else
          let mod_pre, mod_post = split_mod mod_body in
          Seq (binding, insert (replace_ph mod_pre acc) mod_post))
      body mods
  in
  Field.fset Func.Fields.body f body'
;;

let inline_cnstr_calls_f (cnstrs : Func.t list) (f : Func.t) : Func.t =
  if not (Func.is_constructor f) then
    let () = assert (List.is_empty f.Func.info.mod_list2) in
    f
  else
    let body = f.body in
    let mods = List.rev f.info.mod_list2 in
    let body' =
      List.fold_left
        (fun acc (m : Mod_call.t) ->
          let cnstr = List.find (fun (f : Func.t) -> f.name = m.id) cnstrs in
          let binding = bind_params m.loc cnstr.params m.args in
          let cbody = cnstr.body in
          Stmt.Seq (Seq (binding, cbody), acc))
        body mods
    in
    Field.fset Func.Fields.body f body'
;;

let inline_mods_c : Func.t list -> Contract.t -> Contract.t =
 fun cnstrs c ->
  let funcs = c.funcs in
  let funcs = List.map (inline_mod_calls_f funcs) funcs in
  let funcs = List.map (inline_cnstr_calls_f cnstrs) funcs in
  { c with funcs }
;;

let inline_mod_calls (p : Pgm.t) : Pgm.t =
  let cnstrs = List.map Contract.cnstr p in
  List.map (inline_mods_c cnstrs) p
;;

(************************************)
(************************************)
(** return variable initialization **)
(************************************)
(************************************)

let add_retvar_init_f (f : Func.t) : Func.t =
  let new_stmt =
    List.fold_left
      (fun acc (x, xinfo) ->
        let s = Stmt.Decl (Var (x, xinfo)) in
        if Stmt.is_skip acc then s else Seq (acc, s))
      Skip f.ret_params
  in
  let body = f.body in
  let new_body = if Stmt.is_skip new_stmt then body else Seq (new_stmt, body) in
  Field.fset Func.Fields.body f new_body
;;

let add_retvar_init_c (c : Contract.t) : Contract.t =
  let funcs = c.funcs in
  let funcs = List.map add_retvar_init_f funcs in
  { c with funcs }
;;

let add_retvar_init_p (contracts : Pgm.t) : Pgm.t =
  List.map
    (fun c ->
      if c.Contract.info.ckind = Library then c
      (* for optimization, do not introduce additional stmt for lib functions. *)
        else add_retvar_init_c c)
    contracts
;;

let add_retvar_init : Pgm.t -> Pgm.t = fun p -> add_retvar_init_p p

(***********************)
(***********************)
(** Variable Renaming **)
(***********************)
(***********************)

let do_not_rename (id, vinfo) =
  String.starts_with id tmpvar
  || String.starts_with id Translator.param_name (* ghost ret param names are already unique *)
  || vinfo.refid = -1 (* some built-in variables do not have reference id, thus we assign '-1' *)
;;

let rec rename_lv cnames (enums : Enums.t list) lv =
  let enums_assoc = List.map (fun e -> Enums.(e.id, e.members)) enums in
  match lv with
  | Var (id, vinfo) ->
    if do_not_rename (id, vinfo) then lv else Var (id ^ separator ^ string_of_int vinfo.refid, vinfo)
  | MemberAccess (Lv (Var (x, xt)), id, id_info, typ)
    when Typ.is_enum typ && List.mem x (List.map fst enums_assoc) ->
    let members = List.assoc x enums_assoc in
    let idx = Option.get (List.index_of id members) in
    MemberAccess (Lv (Var (x, xt)), id ^ "__idx" ^ string_of_int idx, id_info, typ)
  | MemberAccess (Lv (MemberAccess (e, fname, finfo, typ1)), "selector", sinfo, typ2) ->
    MemberAccess
      (Lv (MemberAccess (rename_e cnames enums e, fname, finfo, typ1)), "selector", sinfo, typ2)
  | MemberAccess (e, id, id_info, typ) ->
    let id' =
      if do_not_rename (id, id_info) then id else id ^ separator ^ string_of_int id_info.refid
    in
    MemberAccess (rename_e cnames enums e, id', id_info, typ)
  | IndexAccess (_, None, _) -> failwith "rename_lv cnames enums1"
  | IndexAccess (e1, Some e2, typ) ->
    IndexAccess (rename_e cnames enums e1, Some (rename_e cnames enums e2), typ)
  | Tuple (eoplst, typ) ->
    let eoplst' =
      List.map
        (fun eop -> match eop with None -> None | Some e -> Some (rename_e cnames enums e))
        eoplst
    in
    Tuple (eoplst', typ)

and rename_e cnames enums exp =
  match exp with
  | V _ | Str _ -> exp
  | Lv lv ->
    if List.mem (to_string_lv lv) Lang.keyword_vars then Lv lv else Lv (rename_lv cnames enums lv)
  | Cast (typ, e) -> Cast (typ, rename_e cnames enums e)
  | BinOp (bop, e1, e2, einfo) ->
    BinOp (bop, rename_e cnames enums e1, rename_e cnames enums e2, einfo)
  | UnOp (uop, e, typ) -> UnOp (uop, rename_e cnames enums e, typ)
  | Ite (i, t, e, typ) ->
    Ite (rename_e cnames enums i, rename_e cnames enums t, rename_e cnames enums e, typ)
  | ETypeName _ -> exp
  | IndexRangeAccess (base, sop, fop, einfo) ->
    let base' = rename_lv cnames enums base in
    let sop' = match sop with None -> None | Some s -> Some (rename_e cnames enums s) in
    let fop' = match fop with None -> None | Some f -> Some (rename_e cnames enums f) in
    IndexRangeAccess (base', sop', fop', einfo)
  | TypeInfo _ -> exp
  | IncTemp (e, b, l) -> IncTemp (rename_e cnames enums e, b, l)
  | DecTemp (e, b, l) -> DecTemp (rename_e cnames enums e, b, l)
  | CallTemp (e, exps, ethop, gasop, einfo) ->
    let e', exps', ethop', gasop' = rename_call_rhs cnames enums (e, exps, ethop, gasop) in
    CallTemp (e', exps', ethop', gasop', einfo)
  | CondTemp (e1, e2, e3, typ, loc) ->
    CondTemp (rename_e cnames enums e1, rename_e cnames enums e2, rename_e cnames enums e3, typ, loc)
  | AssignTemp (lv, e, loc) -> AssignTemp (rename_lv cnames enums lv, rename_e cnames enums e, loc)

and rename_call_rhs cnames enums (e, exps, ethop, gasop) =
  let e' =
    match e with
    | e when List.mem (to_string_exp e) built_in_funcs -> e
    | Lv (Var _) -> e
    | Lv (MemberAccess (Lv (Var (prefix, _)), _fname, _fname_info, _typ))
    (* safemath.add(...) *)
      when List.mem prefix cnames || prefix = "super" ->
      e
    | Lv (MemberAccess (arg, fname, info, typ)) ->
      (* x.add(...), x[y].add(...) *)
      let arg' = rename_e cnames enums arg in
      Lv (MemberAccess (arg', fname, info, typ))
    | _ -> e
  in
  let exps' =
    let fname = to_string_exp e in
    if List.mem fname [ "struct_init"; "struct_init2"; "contract_init" ] then
      List.hd exps :: List.map (rename_e cnames enums) (List.tl exps)
      (* Rule: the first arg is contract/struct name *)
    else List.map (rename_e cnames enums) exps
  in
  let ethop' = match ethop with None -> ethop | Some e -> Some (rename_e cnames enums e) in
  let gasop' = match gasop with None -> gasop | Some e -> Some (rename_e cnames enums e) in
  (e', exps', ethop', gasop')
;;

let rename_extcall_rhs cnames enums (target, args, ether, gas) =
  let target = rename_e cnames enums target in
  let exps = List.map (rename_e cnames enums) args in
  let ether = Option.map (rename_e cnames enums) ether in
  let gas = Option.map (rename_e cnames enums) gas in
  (target, exps, ether, gas)
;;

let rec rename_s cnames (enums : Enums.t list) (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Assign (lv, exp, loc) -> Assign (rename_lv cnames enums lv, rename_e cnames enums exp, loc)
  | Decl lv -> Decl (rename_lv cnames enums lv)
  | Seq (s1, s2) -> Seq (rename_s cnames enums s1, rename_s cnames enums s2)
  | Call (lvop, e, exps, ethop, gasop, loc) ->
    let lvop' = match lvop with None -> lvop | Some lv -> Some (rename_lv cnames enums lv) in
    let e', exps', ethop', gasop' = rename_call_rhs cnames enums (e, exps, ethop, gasop) in
    Call (lvop', e', exps', ethop', gasop', loc)
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    let ret = Option.map (rename_lv cnames enums) ret in
    let target, args, ether, gas = rename_extcall_rhs cnames enums (target, args, ether, gas) in
    Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }
  | Skip -> Skip
  | If (e, s1, s2, i) ->
    If (rename_e cnames enums e, rename_s cnames enums s1, rename_s cnames enums s2, i)
  | While (e, s) -> While (rename_e cnames enums e, rename_s cnames enums s)
  | Break | Continue | Return (None, _) -> stmt
  | Return (Some e, loc) -> Return (Some (rename_e cnames enums e), loc)
  | Revert -> Revert
  | Assume (e, loc) -> Assume (rename_e cnames enums e, loc)
  | Assert (e, vtyp, loc) -> Assert (rename_e cnames enums e, vtyp, loc)
  | Assembly (lst, loc) ->
    Assembly (List.map (fun (x, refid) -> (x ^ separator ^ string_of_int refid, refid)) lst, loc)
  | Placeholder -> Placeholder
  | Unchecked (slst, loc) ->
    let slst' = List.map (rename_s cnames enums) slst in
    Unchecked (slst', loc)
  | Label _ -> stmt
;;

let rename_param (id, vinfo) =
  if String.starts_with id Translator.param_name then (id, vinfo)
  else if Typ.is_func vinfo.vtyp then (id, vinfo)
  else (id ^ separator ^ string_of_int vinfo.refid, vinfo)
;;

let rename_f cnames enums f =
  let f = Field.map Func.Fields.params ~f:(List.map rename_param) f in
  let f = Field.map Func.Fields.ret_params ~f:(List.map rename_param) f in
  let f = Field.map Func.Fields.body ~f:(rename_s cnames enums) f in
  f
;;

let rename_d (d : State_var_decl.t) : State_var_decl.t =
  let name = d.name ^ separator ^ string_of_int d.vinfo.refid in
  { d with name }
;;

let rename_st (s : Struct.t) =
  let fields =
    List.map
      Var2.(fun { id; info } -> { id = id ^ separator ^ string_of_int info.refid; info })
      s.fields
  in
  { s with fields }
;;

let rename_c cnames (c : Contract.t) =
  {
    c with
    decls = List.map rename_d c.decls;
    structs = List.map rename_st c.structs;
    funcs = List.map (rename_f cnames c.enums) c.funcs;
  }
;;

let rename_p p =
  let cnames = Pgm.cnames p in
  List.map (rename_c cnames) p
;;

let rename pgm = rename_p pgm

let tuple_assign lv exp loc : Stmt.t =
  match (lv, exp) with
  | Tuple (eops1, typ1), Lv (Tuple (eops2, _)) when List.length eops1 <> List.length eops2 -> begin
    match List.hd eops1 with
    | Some _ ->
      let eops1', _ =
        list_fold
          (fun _e (acc, acc_index) ->
            if acc_index = 0 then (acc @ [ None ], acc_index) else (acc, acc_index - 1))
          eops2
          (eops1, List.length eops1)
      in
      Assign (Tuple (eops1', typ1), exp, loc)
    | None ->
      let eops1' = List.tl eops1 in
      let eops1'', _ =
        list_fold
          (fun _e (acc, acc_index) ->
            if acc_index = 0 then (acc, acc_index) else (None :: acc, acc_index - 1))
          eops2
          (eops1', List.length eops2 - List.length eops1')
      in
      Assign (Tuple (eops1'', typ1), exp, loc)
  end
  (* (bool success, ) = recipient.call.value(amountToWithdraw)(""); 
  => (succcess, ) := Tmp 
  => success := Tmp *)
  | Tuple ([ Some (Lv lv1); None ], _), Lv lv2 -> Assign (lv1, Lv lv2, loc)
  | _ -> Assign (lv, exp, loc)
;;

let rec tuple_s (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Assign (lv, exp, loc) -> tuple_assign lv exp loc
  | Decl (Tuple (eops, _)) ->
    List.fold_left
      (fun acc eop ->
        match eop with
        | None -> acc
        | Some (Lv lv) -> Stmt.Seq (acc, Decl lv)
        | Some _ -> assert false)
      Skip eops
  | Seq (s1, s2) -> Seq (tuple_s s1, tuple_s s2)
  | If (e, s1, s2, i) -> If (e, tuple_s s1, tuple_s s2, i)
  | While (e, s) -> While (e, tuple_s s)
  | _ -> stmt
;;

let tuple_f (f : Func.t) = Field.fset Func.Fields.body f (tuple_s f.body)
let tuple_c c = Field.map Contract.Fields.funcs ~f:(List.map tuple_f) c
let extend_tuple pgm = List.map tuple_c pgm

(***************************************************************)
(**** Add guards for arithmetic operations (solv >= 0.8.0), ****)
(**** division (non-zero), array access (length > 0)        ****)
(***************************************************************)

(* Reference for division: https://github.com/Z3Prover/z3/issues/2843 *)
let rec add_io_dz_e exp =
  let solc_ver = Options.get_solc_ver () in
  match exp with
  | V _ | Str _ -> []
  | Lv lv -> add_io_dz_lv lv
  | Cast (_, e) -> add_io_dz_e e
  | BinOp (Add, e1, e2, einfo) when solc_ver >= Solc.Ver.mk 0 8 0 ->
    let res = (mk_ge (mk_add e1 e2) e1, einfo.loc) in
    ("io", res) :: (add_io_dz_e e1 @ add_io_dz_e e2)
  | BinOp (Sub, e1, e2, einfo) when solc_ver >= Solc.Ver.mk 0 8 0 ->
    let res = (mk_ge e1 e2, einfo.loc) in
    ("io", res) :: (add_io_dz_e e1 @ add_io_dz_e e2)
  | BinOp (Mul, e1, e2, einfo)
    when solc_ver >= Solc.Ver.mk 0 8 0
         (* e.g., (1/100000) * (10**18) is not considered *)
         && (Typ.is_const_int (get_type_exp e1)
            || Typ.is_uintkind (get_type_exp e1)
            || Typ.is_sintkind (get_type_exp e1))
         && (Typ.is_const_int (get_type_exp e2)
            || Typ.is_uintkind (get_type_exp e2)
            || Typ.is_sintkind (get_type_exp e2)) ->
    let zero = mk_eq e1 (V (Int Z.zero)) in
    let not_zero = mk_neq e1 (V (Int Z.zero)) in
    let mul_div = mk_div exp e1 in
    let res = (mk_or zero (mk_and not_zero (mk_eq mul_div e2)), einfo.loc) in
    ("io", res) :: (add_io_dz_e e1 @ add_io_dz_e e2)
  | BinOp (Div, e1, e2, einfo) ->
    let res = (mk_neq e2 (V (Int Z.zero)), einfo.loc) in
    ("dz", res) :: (add_io_dz_e e1 @ add_io_dz_e e2)
  | BinOp (_, e1, e2, _) -> add_io_dz_e e1 @ add_io_dz_e e2
  | UnOp (_, e, _) -> add_io_dz_e e
  | Ite (i, t, e, _) -> add_io_dz_e i @ add_io_dz_e t @ add_io_dz_e e
  | ETypeName _ -> []
  | IndexRangeAccess (base, sop, fop, _) ->
    let lst1 = add_io_dz_lv base in
    let lst2 = match sop with Some s -> add_io_dz_e s | None -> [] in
    let lst3 = match fop with Some f -> add_io_dz_e f | None -> [] in
    lst1 @ lst2 @ lst3
  | TypeInfo _ -> []
  | IncTemp _ | DecTemp _ | CallTemp _ | CondTemp _ | AssignTemp _ -> failwith "add_io_dz_e"

and add_io_dz_lv lv =
  match lv with
  | Var _ -> []
  | MemberAccess (e, _, _, _) -> add_io_dz_e e
  | IndexAccess (e, None, _t) -> add_io_dz_e e
  | IndexAccess (e1, Some e2, _t) -> add_io_dz_e e1 @ add_io_dz_e e2
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop -> match eop with None -> acc | Some e -> acc @ add_io_dz_e e)
      [] eops
;;

(** vaa: valid array access. E.g., arr[i] => arr.length > i *)
let rec add_vaa_e exp =
  match exp with
  | V _ | Str _ -> []
  | Lv lv -> add_vaa_lv lv
  | Cast (_, e) -> add_vaa_e e
  | BinOp (_, e1, e2, _) -> add_vaa_e e1 @ add_vaa_e e2
  | UnOp (_, e, _) -> add_vaa_e e
  | Ite (i, t, e, _) -> add_vaa_e i @ add_vaa_e t @ add_vaa_e e
  | ETypeName _ | IndexRangeAccess _ | TypeInfo _ -> []
  | IncTemp _ | DecTemp _ | CallTemp _ | CondTemp _ | AssignTemp _ -> failwith "add_vaa_e"

and add_vaa_lv lv =
  match lv with
  | Var _ -> []
  | MemberAccess (e, _, _, _) -> add_vaa_e e
  | IndexAccess (e, None, _) -> add_vaa_e e
  | IndexAccess (e1, Some e2, _) ->
    if Typ.is_array (get_type_exp e1) then
      let res = (mk_gt (mk_member_access e1 ("length", EType (UInt 256))) e2, Loc.dummy) in
      ("vaa", res) :: (add_vaa_e e1 @ add_vaa_e e2)
    else add_vaa_e e1 @ add_vaa_e e2
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop -> match eop with None -> acc | Some e -> acc @ add_vaa_e e)
      [] eops
;;

(** add assertions within unchecked blocks *)
let rec add_assert_unchecked (stmt : Stmt.t) =
  let add_a' acc (kind, (x, l)) =
    if kind = "io" then Stmt.Seq (Assert (x, "io", l), acc) else acc
  in
  match stmt with
  | Assign (lv, e, _loc) ->
    let lst = add_io_dz_lv lv @ add_io_dz_e e in
    List.fold_left add_a' stmt lst
  | Decl _ -> stmt
  | Seq (s1, s2) -> Seq (add_assert_unchecked s1, add_assert_unchecked s2)
  | Call (lvop, e, args, ethop, gasop, _loc) ->
    let lvop_lst = match lvop with None -> [] | Some lv -> add_io_dz_lv lv in
    let e_lst = add_io_dz_e e in
    let args_lst = List.fold_left (fun acc e' -> acc @ add_io_dz_e e') [] args in
    let ethop_lst = match ethop with None -> [] | Some e -> add_io_dz_e e in
    let gasop_lst = match gasop with None -> [] | Some e -> add_io_dz_e e in
    let lst = lvop_lst @ e_lst @ args_lst @ ethop_lst @ gasop_lst in
    List.fold_left add_a' stmt lst
  | Extcall _ ->
    Pp.invalid_argf "preprocess.ml : Extcall constructors appear only in preprocess phase 2"
  | Skip -> stmt
  | If (e, s1, s2, i) ->
    let lst = add_io_dz_e e in
    if List.length lst = 0 then If (e, add_assert_unchecked s1, add_assert_unchecked s2, i)
    else
      let s' = List.fold_left add_a' Skip lst in
      Seq (s', If (e, add_assert_unchecked s1, add_assert_unchecked s2, i))
  | While (e, s) ->
    let lst = add_io_dz_e e in
    if List.length lst = 0 then While (e, add_assert_unchecked s)
    else
      let s' = List.fold_left add_a' Skip lst in
      Seq (s', While (e, add_assert_unchecked s))
  | Break | Continue -> stmt
  | Return (None, _) -> stmt
  | Return (Some e, _) ->
    let lst = add_io_dz_e e in
    List.fold_left add_a' stmt lst
  | Revert -> stmt
  | Assume (e, _loc) ->
    let lst = add_io_dz_e e in
    List.fold_left add_a' stmt lst
  | Assert (e, "default", _loc) ->
    let lst = add_io_dz_e e in
    List.fold_left add_a' stmt lst
  | Assert _ -> stmt (* automatically inserted assertion *)
  | Assembly _ | Placeholder -> stmt
  | Unchecked _ -> assert false
  | Label _ -> stmt
;;

let rec add_assume_s ?(uc = false) (stmt : Stmt.t) : Stmt.t =
  let add_g' acc (kind, (e, _)) : Stmt.t =
    if kind = "io" && not uc then Seq (Assume (e, Loc.dummy), acc)
    else if kind = "io" && uc then acc
    else Seq (acc, Assume (e, Loc.dummy))
  in
  match stmt with
  | Assign (lv, e, _loc) ->
    let lst = add_io_dz_lv lv @ add_io_dz_e e @ add_vaa_lv lv @ add_vaa_e e in
    List.fold_left add_g' stmt lst
  | Decl _ -> stmt
  | Seq (s1, s2) -> Seq (add_assume_s ~uc s1, add_assume_s ~uc s2)
  | Call (lvop, e, args, ethop, gasop, _loc) ->
    let lvop_lst = match lvop with None -> [] | Some lv -> add_io_dz_lv lv @ add_vaa_lv lv in
    let e_lst = add_io_dz_e e @ add_vaa_e e in
    let args_lst = List.fold_left (fun acc e' -> acc @ add_io_dz_e e' @ add_vaa_e e') [] args in
    let ethop_lst = match ethop with None -> [] | Some e -> add_io_dz_e e @ add_vaa_e e in
    let gasop_lst = match gasop with None -> [] | Some e -> add_io_dz_e e @ add_vaa_e e in
    let lst = lvop_lst @ e_lst @ args_lst @ ethop_lst @ gasop_lst in
    List.fold_left add_g' stmt lst
  | Extcall _ ->
    Pp.invalid_argf "preprocess.ml : Extcall constructors appear only in preprocess phase 2"
  | Skip -> stmt
  | If (e, s1, s2, i) ->
    let lst = add_io_dz_e e @ add_vaa_e e in
    if List.length lst = 0 then (* just for readability of IL *)
      If (e, add_assume_s ~uc s1, add_assume_s ~uc s2, i)
    else
      let s' = List.fold_left add_g' Skip lst in
      Seq (s', If (e, add_assume_s ~uc s1, add_assume_s ~uc s2, i))
  | While (e, s) ->
    let lst = add_io_dz_e e @ add_vaa_e e in
    if List.length lst = 0 then (* just for readability of IL *)
      While (e, add_assume_s ~uc s)
    else
      let s' = List.fold_left add_g' Skip lst in
      Seq (s', While (e, add_assume_s ~uc s))
    (* Seq (While (e, Seq (s', add_assume_s ~mode s)), s') *)
  | Break | Continue -> stmt
  | Return _ | Revert -> stmt
  | Assume (e, _loc) ->
    let lst = add_io_dz_e e @ add_vaa_e e in
    List.fold_left add_g' stmt lst
  | Assert (e, "default", _loc) ->
    let lst = add_io_dz_e e @ add_vaa_e e in
    List.fold_left add_g' stmt lst
  | Assert (_e, _, _loc) -> stmt (* automatically inserted assertion *)
  | Assembly _ | Placeholder -> stmt
  | Unchecked (slst, _loc) ->
    let slst = List.map (add_assume_s ~uc:true) slst in
    let slst = List.map add_assert_unchecked slst in
    List.fold_left (fun acc s -> if Stmt.is_skip acc then s else Seq (acc, s)) Skip slst
  | Label _ -> stmt
;;

let add_assume_f (f : Func.t) = Field.fset Func.Fields.body f (add_assume_s f.body)
let add_assume_c c = Field.map Contract.Fields.funcs ~f:(List.map add_assume_f) c
let add_assume pgm = List.map add_assume_c pgm

(*****************************)
(**** Desugar struct_init ****)
(*****************************)

(* NOTE: # of 'members' & 'member_values' can be different, when there exist mapping-typed members *)
(* NOTE: Therefore, implemented a customized fold_left2 function. *)
let rec fold_left2 lv loc acc (members : Var2.t list) values =
  match (members, values) with
  | [], [] -> acc
  | [], _ :: _ -> invalid_arg "preprocess: desugar struct init"
  | h1 :: t1, [] ->
    if Typ.is_mapping h1.info.vtyp then
      let lv' = MemberAccess (Lv lv, h1.id, h1.info, h1.info.vtyp) in
      let stmt' = Stmt.Decl lv' in
      let stmt'' = if Stmt.is_skip acc then stmt' else Seq (acc, stmt') in
      fold_left2 lv loc stmt'' t1 []
    else invalid_arg "preprocess: desugar struct init"
  | h1 :: t1, h2 :: t2 ->
    if Typ.is_mapping h1.info.vtyp then
      let lv' = MemberAccess (Lv lv, h1.id, h1.info, h1.info.vtyp) in
      let stmt' = Stmt.Decl lv' in
      let stmt'' = if Stmt.is_skip acc then stmt' else Seq (acc, stmt') in
      fold_left2 lv loc stmt'' t1 (h2 :: t2)
    else
      let lv' = MemberAccess (Lv lv, h1.id, h1.info, h1.info.vtyp) in
      let stmt' = Stmt.Assign (lv', h2, loc) in
      let stmt'' = if Stmt.is_skip acc then stmt' else Seq (acc, stmt') in
      fold_left2 lv loc stmt'' t1 t2
;;

let rec dsg cname smap (stmt : Stmt.t) =
  match stmt with
  | Assign _ | Decl _ -> stmt
  | Seq (s1, s2) -> Seq (dsg cname smap s1, dsg cname smap s2)
  | Call (Some lv, Lv (Var ("struct_init", _)), args, _ethop, _gasop, loc) ->
    let struct_exp, member_values = (List.hd args, List.tl args) in
    (* Types of type-name-expressions are their type-names. E.g., typeOf (StructName) => ContractName.StructName *)
    (* see the implementation in frontend/translator.ml *)
    let members = StructMap.find (Typ.get_name_userdef (get_type_exp struct_exp)) smap in
    fold_left2 lv loc Skip members member_values
  | Call (Some lv, Lv (Var ("struct_init2", _)), args, _, _, loc) ->
    let args1, args2 = List.split_at ((List.length args / 2) + 1) args in
    let struct_exp, member_names, member_values = (List.hd args1, List.tl args1, args2) in
    let org_members = StructMap.find (Typ.get_name_userdef (get_type_exp struct_exp)) smap in
    let find_matching_member mname member_list =
      List.find (fun d -> String.starts_with d.Var2.id (to_string_exp mname)) member_list
    in
    let members = List.map (fun name -> find_matching_member name org_members) member_names in
    fold_left2 lv loc Skip members member_values
  | Extcall _ ->
    Pp.invalid_argf "preprocess.ml : Extcall constructors appear only in preprocess phase 2"
  | Call _ | Skip -> stmt
  | If (e, s1, s2, i) -> If (e, dsg cname smap s1, dsg cname smap s2, i)
  | While (e, s) -> While (e, dsg cname smap s)
  | Break | Continue | Return _ | Revert | Assume _ | Assert _ | Assembly _ | Placeholder -> stmt
  | Unchecked (lst, _loc) ->
    let lst' = List.map (dsg cname smap) lst in
    List.fold_left (fun acc s -> if Stmt.is_skip acc then s else Seq (acc, s)) Skip lst'
  | Label _ -> stmt
;;

let desugar_struct_f cname smap (f : Func.t) = Field.fset Func.Fields.body f (dsg cname smap f.body)

let desugar_struct_c smap c =
  let cname = Contract.name c in
  Field.map Contract.Fields.funcs ~f:(List.map (desugar_struct_f cname smap)) c
;;

let desugar_struct pgm =
  let smap = StructMap.mk_smap pgm in
  List.map (desugar_struct_c smap) pgm
;;

let run (p : Pgm.t) : Pgm.t =
  let p = copy p in
  let p = inline_mod_calls p in
  (* after 'copy' *)
  let p = move_decl_to_cnstr p in
  (* after 'copy' *)
  let p = replace_tmpexp p in
  (* after 'inline_mod_calls'; due to function call expressions (callTemp) as modifier arguments *)
  let p = normalize p in
  let p = rmskip p in
  let p = replace_lib_calls p in
  let p = add_cname_fcalls p in
  (* after 'copy' *)
  let p = add_getter p in
  let p = rmskip p in
  let p = replace_super p in
  let p = rmskip p in
  let p = rename p in
  let p = add_retvar_init p in
  (* let p = add_ret p in *)
  let p = extend_tuple p in
  let p = add_assume p in
  let p = desugar_struct p in
  let p = rmskip p in
  p
;;

module Unused = struct
  let rec has_cnstr_calls_s (cnstrs : Func.t list) (stmt : Stmt.t) : bool =
    match stmt with
    | Assign _ -> false
    | Seq (s1, s2) -> has_cnstr_calls_s cnstrs s1 || has_cnstr_calls_s cnstrs s2
    | Decl _ -> false
    | Call (None, Lv (Var (f, _)), _, _, _, _)
      when List.mem f (List.map (fun f -> f.Func.name) cnstrs) ->
      true
    | Call _ -> false
    | Extcall _ -> false
    | Skip -> false
    | Assume _ -> false
    | While (_, s) -> has_cnstr_calls_s cnstrs s
    | If (_, s1, s2, _) -> has_cnstr_calls_s cnstrs s1 || has_cnstr_calls_s cnstrs s2
    | Continue | Break | Return _ | Revert | Assert _ | Assembly _ | Placeholder -> false
    | Unchecked (slst, _) -> List.exists (has_cnstr_calls_s cnstrs) slst
    | Label _ -> false
  ;;

  let has_cnstr_calls_f (cnstrs : Func.t list) (f : Func.t) : bool =
    if Func.is_constructor f then has_cnstr_calls_s cnstrs (Func.body f) else false
  ;;

  let has_cnstr_calls_c cnstrs c = List.exists (has_cnstr_calls_f cnstrs) (Contract.funcs c)

  let has_cnstr_calls_p p =
    let cnstrs = List.map Contract.cnstr p in
    List.exists (has_cnstr_calls_c cnstrs) p
  ;;

  let[@warning "-32"] has_cnstr_calls p = has_cnstr_calls_p p

  let add_ret_s f (s : Stmt.t) : Stmt.t =
    try
      let lv = params_to_lv f.Func.ret_params in
      Seq (s, Return (Some (Lv lv), Loc.dummy))
    with NoParameters -> Seq (s, Return (None, Loc.dummy))
  ;;

  let add_ret_f f = Field.fset Func.Fields.body f (add_ret_s f f.body)
  let add_ret_c c = Contract.{ c with funcs = List.map add_ret_f c.funcs }

  (** add return statement at the exit. *)
  let[@warning "-32"] add_ret pgm = List.map add_ret_c pgm

  let rec cast_lv lv =
    match lv with
    | Var _ -> lv
    | MemberAccess (e, x, xinfo, typ) -> MemberAccess (cast_e e, x, xinfo, typ)
    | IndexAccess (e, None, typ) -> IndexAccess (cast_e e, None, typ)
    | IndexAccess (e1, Some e2, typ) ->
      let expected_idx_typ = Typ.domain (get_type_exp e1) in
      let idx_typ = get_type_exp e2 in
      let e1' = cast_e e1 in
      let e2' =
        if expected_idx_typ = idx_typ then cast_e e2 else Cast (expected_idx_typ, cast_e e2)
      in
      IndexAccess (e1', Some e2', typ)
    | Tuple (eoplst, typ) ->
      let eoplst' =
        List.map (fun eop -> match eop with Some e -> Some (cast_e e) | None -> None) eoplst
      in
      Tuple (eoplst', typ)

  and cast_e exp =
    match exp with
    | V _ | Str _ -> exp
    | Lv lv -> Lv (cast_lv lv)
    | Cast (typ, e) -> Cast (typ, cast_e e)
    | BinOp (bop, e1, e2, einfo) ->
      let t1 = get_type_exp e1 in
      let t2 = get_type_exp e2 in
      let ctyp = common_typ e1 e2 in
      let e1' = if t1 = ctyp then cast_e e1 else Cast (ctyp, cast_e e1) in
      let e2' = if t2 = ctyp then cast_e e2 else Cast (ctyp, cast_e e2) in
      BinOp (bop, e1', e2', einfo)
    | UnOp (uop, e, typ) -> UnOp (uop, cast_e e, typ)
    | Ite (i, t, e, typ) -> Ite (cast_e i, cast_e t, cast_e e, typ)
    | ETypeName _ -> exp
    | IndexRangeAccess (base, startop, finop, einfo) ->
      let f eop = match eop with Some e -> Some (cast_e e) | None -> None in
      IndexRangeAccess (cast_lv base, f startop, f finop, einfo)
    | TypeInfo _ -> exp
    | IncTemp _ | DecTemp _ | CallTemp _ | CondTemp _ | AssignTemp _ -> failwith "cast_e"

  and cast_s (stmt : Stmt.t) : Stmt.t =
    match stmt with
    | Assign (lv, e, loc) ->
      let lv_typ = get_type_lv lv in
      let e_typ = get_type_exp e in
      let lv' = cast_lv lv in
      let e' = if lv_typ = e_typ then cast_e e else Cast (lv_typ, cast_e e) in
      Assign (lv', e', loc)
    | Decl _ -> stmt
    | Seq (s1, s2) -> Seq (cast_s s1, cast_s s2)
    | Call (lvop, e, args, ethop, gasop, loc) ->
      let lvop' = match lvop with Some lv -> Some (cast_lv lv) | None -> None in
      let e' = cast_e e in
      let elst' = List.map cast_e args in
      let ethop' = match ethop with Some e -> Some (cast_e e) | None -> None in
      let gasop' = match gasop with Some e -> Some (cast_e e) | None -> None in
      Call (lvop', e', elst', ethop', gasop', loc)
    | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
      let ret = Option.map cast_lv ret in
      let target = cast_e target in
      let args = List.map cast_e args in
      let ether = Option.map cast_e ether in
      let gas = Option.map cast_e gas in
      Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }
    | If (e1, s1, s2, i) -> If (cast_e e1, cast_s s1, cast_s s2, i)
    | While (e, s) -> While (cast_e e, cast_s s)
    | Skip | Break | Continue | Return _ | Revert | Assembly _ | Placeholder | Label _ -> stmt
    | Assume (e, loc) -> Assume (cast_e e, loc)
    | Assert (e, vtyp, loc) -> Assert (cast_e e, vtyp, loc)
    | Unchecked (slst, loc) -> Unchecked (List.map cast_s slst, loc)
  ;;

  let cast_f (f : Func.t) = Field.fset Func.Fields.body f (cast_s f.body)
  let cast_c c = Contract.{ c with funcs = List.map cast_f c.funcs }

  (** casting? *)
  let[@warning "-32"] cast pgm = List.map cast_c pgm
end
