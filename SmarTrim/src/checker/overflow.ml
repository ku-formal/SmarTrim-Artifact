open Vlang
open Frontend.Lang
open Options
open Semantics

let gen_safety_cond (exp : exp) : vformula =
  match exp with
  | BinOp (bop, e1, e2, _einfo) ->
    let typ = get_type_exp exp in
    let ve1 = convert_aexp e1 in
    let ve2 = convert_aexp e2 in
    begin
      match bop with
      | Add -> VBinRel (VGeq, VBinOp (VAdd, ve1, ve2, typ), ve1)
      | Sub -> VBinRel (VGeq, ve1, ve2)
      | Mul ->
        let zero =
          VBinRel (VEq, ve1, VInt Z.zero)
          (* a=0 *)
        in
        let mul = VBinOp (VMul, ve1, ve2, typ) in
        (* a*b *)
        let mul_div = VBinOp (VDiv, mul, ve1, typ) in
        (* (a*b)/a *)
        VOr (zero, VBinRel (VEq, mul_div, ve2))
      | Div -> VNot (VBinRel (VEq, ve2, VInt Z.zero))
      | Mod -> VNot (VBinRel (VEq, ve2, VInt Z.zero))
      | Exponent ->
        (* 10**50 or 2 ** 255 *)
        let f1 = VBinRel (VGeq, VInt (Z.of_int 10), ve1) in
        let f2 = VBinRel (VGeq, VInt (Z.of_int 50), ve2) in
        let f3 = VBinRel (VGeq, VInt (Z.of_int 2), ve1) in
        let f4 = VBinRel (VGeq, VInt (Z.of_int 255), ve2) in
        let f5 = VBinRel (VGeq, VInt (Z.of_int 256), ve1) in
        let f6 = VBinRel (VGeq, VInt (Z.of_int 31), ve2) in
        VOr (VAnd (f1, f2), VOr (VAnd (f3, f4), VAnd (f5, f6)))
      | ShiftL -> VBinRel (VGeq, VBinOp (VShiftL, ve1, ve2, typ), ve1)
      | ShiftR -> VBinRel (VGeq, ve1, VBinOp (VShiftR, ve1, ve2, typ))
      | _ -> raise (Failure "gen_safety_cond: not related ops1")
    end
  | _ -> raise (Failure "gen_safety_cond: not related ops2")
;;

let gen_safety_cond_src (exp : exp) =
  match exp with
  | BinOp (bop, e1, e2, _einfo) -> (
    let typ = get_type_exp exp in
    match bop with
    | Add -> BinOp (GEq, exp, e1, mk_einfo typ)
    | Sub -> BinOp (GEq, e1, e2, mk_einfo typ)
    | Mul ->
      let zero = mk_eq e1 (V (Int Z.zero)) in
      let not_zero = mk_neq e1 (V (Int Z.zero)) in
      let mul_div = BinOp (Div, exp, e1, mk_einfo typ) in
      mk_or zero (mk_and not_zero (mk_eq mul_div e2))
    | Div -> mk_neq e2 (V (Int Z.zero))
    | Mod -> mk_neq e2 (V (Int Z.zero))
    | Exponent ->
      let sc1 = mk_ge (V (Int (Z.of_int 10))) e1 in
      let sc2 = mk_ge (V (Int (Z.of_int 50))) e2 in
      let sc3 = mk_ge (V (Int (Z.of_int 2))) e1 in
      let sc4 = mk_ge (V (Int (Z.of_int 255))) e2 in
      let sc5 = mk_ge (V (Int (Z.of_int 256))) e1 in
      let sc6 = mk_ge (V (Int (Z.of_int 31))) e2 in
      mk_or (mk_and sc1 sc2) (mk_or (mk_and sc3 sc4) (mk_and sc5 sc6))
    (* | ShiftL -> mk_ge exp e1
     | ShiftR -> mk_ge e1 exp *)
    | _ -> failwith "gen_safety_cond_src")
  | _ -> failwith "gen_safety_cond_src2"
;;

let rec collect_queries_exp (vf : vformula) (path : Paths.t) (exp : exp) : Query.t list =
  match exp with
  | V _ | Str _ -> []
  | Lv lv -> collect_queries_lv vf path lv
  | Cast (_, e) -> collect_queries_exp vf path e
  | UnOp (_, e, _) -> collect_queries_exp vf path e
  | BinOp (bop, e1, e2, einfo) -> (
    let lst = List.rev_append (collect_queries_exp vf path e1) (collect_queries_exp vf path e2) in
    match bop with
    | (Add | Sub | Mul | Exponent) when !Chk.io ->
      if einfo.loc.line < 0 then lst
        (* do not collect queries from generated test harness or modeled code *)
      else if !alarm_filter && bop = Exponent then lst
      else if (not !pow) && bop = Exponent then lst
      else if get_solc_ver () >= Solc.Ver.mk 0 8 0 then lst
      else
        let sc = gen_safety_cond exp in
        let sc_src = to_string_exp ~report:true (gen_safety_cond_src exp) in
        let vc = Imply (vf, sc) in
        {
          vc;
          vc2 = sc;
          vc_kind = `Delayed;
          kind = IO;
          qloc = einfo.loc;
          org_q = Org_exp exp;
          path;
          src_f = path.fkey;
          sc_src;
          attacker_src = "";
          eth_src = "";
        }
        :: lst
    | (Div | Mod) when !Chk.dz ->
      if einfo.loc.line < 0 then lst (* do not collect queries from generated test harness *)
      else
        let sc = gen_safety_cond exp in
        let sc_src = to_string_exp ~report:true (gen_safety_cond_src exp) in
        let vc = Imply (vf, sc) in
        {
          vc;
          vc2 = sc;
          vc_kind = `Basic;
          kind = DZ;
          qloc = einfo.loc;
          org_q = Org_exp exp;
          path;
          src_f = path.fkey;
          sc_src;
          attacker_src = "";
          eth_src = "";
        }
        :: lst
    | _ -> lst)
  | ETypeName _ -> []
  | IndexRangeAccess (base, startop, finop, _) ->
    let f eop = match eop with Some e -> collect_queries_exp vf path e | None -> [] in
    collect_queries_lv vf path base @ f startop @ f finop
  | TypeInfo _ -> []
  | _ -> failwith ("collect_queries_exp (tmp expressions encountered) : " ^ to_string_exp exp)

and collect_queries_lv (vf : vformula) (path : Paths.t) (lv : lv) : Query.t list =
  match lv with
  | Var _ -> []
  | MemberAccess (e, _, _, _) -> collect_queries_exp vf path e
  | IndexAccess (e1, Some e2, _) ->
    List.rev_append (collect_queries_exp vf path e1) (collect_queries_exp vf path e2)
  | IndexAccess (_, None, _) -> []
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop ->
        match eop with None -> acc | Some e -> List.rev_append acc (collect_queries_exp vf path e))
      [] eops
;;

let collect_queries_lv_option vf path lv =
  match Option.map (collect_queries_lv vf path) lv with Some l -> l | None -> []
;;

let collect_queries (vf : vformula) (path : Paths.t) (stmt : Stmt.t) : Query.t list =
  match stmt with
  | Assign (lv, e, _) -> collect_queries_lv vf path lv @ collect_queries_exp vf path e
  | Decl lv -> collect_queries_lv vf path lv
  | Call (lv_option, e, args, _, _, _) ->
    collect_queries_lv_option vf path lv_option
    @ collect_queries_exp vf path e
    @ List.fold_left (fun acc e' -> acc @ collect_queries_exp vf path e') [] args
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (id, fkey, ether, gas, is_static, loc);
    collect_queries_lv_option vf path ret
    @ collect_queries_exp vf path target
    @ List.fold_left (fun acc e' -> acc @ collect_queries_exp vf path e') [] args
  | Skip -> []
  | Return (None, _) -> []
  | Return (Some e, _) -> collect_queries_exp vf path e
  | Revert -> []
  | Assume (e, _) -> collect_queries_exp vf path e
  | Assert (e, "io", loc) when !Chk.io ->
    (* generated from unchecked block *)
    let sc = convert_bexp e in
    let sc_src = to_string_exp ~report:true e in
    let vc = Imply (vf, sc) in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Delayed;
        kind = IO;
        qloc = loc;
        org_q = Org_exp e;
        path;
        src_f = path.fkey;
        sc_src;
        attacker_src = "";
        eth_src = "";
      };
    ]
  | Assert (e, _, _) -> collect_queries_exp vf path e
  | Assembly _ | Label _ -> []
  | Seq _ | If _ | While _ | Break | Continue | Placeholder | Unchecked _ ->
    failwith ("overflow:collect_queries : " ^ to_string_stmt stmt)
;;
