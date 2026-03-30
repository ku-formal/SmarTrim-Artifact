open! Frontend
open Frontend.Lang
open Formula

(** Indicates that Z3 Cannot deal with power expression [e.g. 10 ** 18]. *)
exception CannotDealPowerExprs

module Solver_call_reason = struct
  (** Call reason. Used when analyzing overhead time. *)
  type t = INIT | FC | VC | NMC | SC
end

open struct
  let time_statistics : (Solver_call_reason.t, float) Map.t ref =
    Solver_call_reason.[ (INIT, 0.0); (FC, 0.0); (VC, 0.0); (NMC, 0.0); (SC, 0.0) ]
    |> List.to_seq |> Map.of_seq |> ref
  ;;
end

let get_time_statistics reason = Map.find reason !time_statistics

(** Time usage ratio among {b all solver calls}. For example, [get_time_usage_ratio SC] is
    [SC / (INIT + VC + FC + SC)]. Note that it is NOT the ratio w.r.t total time budget. *)
let get_time_usage_ratio (reason : Solver_call_reason.t) : float =
  let x = Map.find reason !time_statistics in
  let sum = Map.values !time_statistics |> Enum.reduce ( +. ) in
  x /. sum
;;

open struct
  let gc_count = ref 0
  let gc_time = ref 0.0

  let[@warning "-32"] full_major () =
    if !gc_count > 200 || not !Options.use_manual_gc then ()
    else begin
      let time = Sys.time () in
      Gc.full_major ();
      let time_spent = Sys.time () -. time in
      gc_time +.= time_spent;
      gc_count += 1
    end
  ;;
end

let get_manual_gc_count () = !gc_count
let get_manual_gc_time () = !gc_time

module Make (ID : sig
  val id : int
end) =
struct
  (** yes, this is an id *)
  let id = ID.id

  (** yes, this is a context *)
  let ctx = Z3.mk_context [ ("timeout", "2147483647") ]

  let timeout_current = ref 2147483647

  let update_timeout timeout =
    if timeout <> !timeout_current then
      Z3.Params.update_param_value ctx "timeout" (string_of_int timeout)
  ;;

  let freshid = ref 0

  let freshvar () =
    incr freshid;
    "tmp" ^ string_of_int !freshid
  ;;

  let rec typ_to_sort (typ : Typ.t) : Z3.Sort.sort =
    match typ with
    | ConstInt -> Z3.Arithmetic.Integer.mk_sort ctx
    | ConstString _ -> Z3.Sort.mk_uninterpreted_s ctx "Uninterpreted-ConstStringSort"
    | ConstReal ->
      invalid_arg "typ_to_sort: ConstReal typed var should be eliminated in preprocessing steps"
    | EType etyp -> etyp_to_sort etyp
    | Mapping (e, t) -> Z3.Z3Array.mk_sort ctx (etyp_to_sort e) (typ_to_sort t)
    | Mapping2 (t1, t2) -> Z3.Z3Array.mk_sort ctx (typ_to_sort t1) (typ_to_sort t2)
    | Array (t, _eop) -> Z3.Z3Array.mk_sort ctx (Z3.BitVector.mk_sort ctx 256) (typ_to_sort t)
    | Struct _ -> Z3.BitVector.mk_sort ctx !Options.struct_bit_usage
    | TupleType _ -> invalid_arg "typ_to_sort: tuple types should not be queried as a whole."
    | FuncType _ -> invalid_arg "typ_to_sort: FuncType"
    | Void -> invalid_arg "typ_to_sort: Void"

  and etyp_to_sort (e : Typ.e) : Z3.Sort.sort =
    match e with
    | Address | AddressPayable | Contract _ -> Z3.BitVector.mk_sort ctx 160
    | Enum _ -> Z3.BitVector.mk_sort ctx 256
    | Bool -> Z3.Boolean.mk_sort ctx
    | String -> Z3.Seq.mk_string_sort ctx
    | UInt n -> Z3.BitVector.mk_sort ctx n
    | SInt n -> Z3.BitVector.mk_sort ctx n
    | Bytes n -> Z3.BitVector.mk_sort ctx (n * 8)
    | DBytes -> Z3.Z3Array.mk_sort ctx (Z3.BitVector.mk_sort ctx 256) (Z3.BitVector.mk_sort ctx 8)
  ;;

  (* NOTE: Convert an expression to match a given target type. *)
  (* NOTE: this function is independent of 'common_typ' function,
  * NOTE: which involves implicit type conversion only.
  * NOTE: In contrast, this function considers explicit type conversion, too. *)
  let rec convert (my_typ : Typ.t) (comp_typ : Typ.t) (z3exp : Z3.Expr.expr) : Z3.Expr.expr =
    if my_typ = comp_typ then z3exp
    else
      match (my_typ, comp_typ) with
      (* extension case *)
      | ConstInt, EType (UInt n) -> Z3.Arithmetic.Integer.mk_int2bv ctx n z3exp
      | ConstInt, EType (SInt n) -> Z3.Arithmetic.Integer.mk_int2bv ctx n z3exp
      | EType (UInt n1), EType (UInt n2) when n1 < n2 ->
        Z3.BitVector.mk_zero_ext ctx (n2 - n1) z3exp
      | EType (SInt n1), EType (SInt n2) when n1 < n2 ->
        Z3.BitVector.mk_sign_ext ctx (n2 - n1) z3exp
      | EType (UInt n1), EType (SInt n2) when n1 <= n2 ->
        Z3.BitVector.mk_sign_ext ctx (n2 - n1) z3exp (* XXX *)
      | EType (SInt n1), EType (UInt n2) when n1 <= n2 ->
        Z3.BitVector.mk_zero_ext ctx (n2 - n1) z3exp (* XXX *)
      | ConstInt, EType Address -> Z3.Arithmetic.Integer.mk_int2bv ctx 160 z3exp
      | ConstInt, EType AddressPayable -> Z3.Arithmetic.Integer.mk_int2bv ctx 160 z3exp
      | ConstInt, EType (Contract _) -> Z3.Arithmetic.Integer.mk_int2bv ctx 160 z3exp
      | ConstInt, EType (Bytes n) ->
        (* e.g., bytes16(17) *)
        Z3.Arithmetic.Integer.mk_int2bv ctx (n * 8) z3exp
      (* shortening case *)
      | EType (UInt n1), EType (UInt n2) when n1 > n2 ->
        Z3.BitVector.mk_extract ctx (n2 - 1) 0 z3exp (* XXX *)
      (* other cases *)
      | EType (Bytes n1), EType (UInt n2) when n1 * 8 <= n2 ->
        (* syntax disallowed since solc 0.5.0 *)
        Z3.BitVector.mk_zero_ext ctx (n2 - (n1 * 8)) z3exp
      | EType Address, EType (Contract _) -> z3exp
      | EType (Contract _), EType (Contract _) -> z3exp
      | EType (Contract _), EType Address ->
        let str = Z3.Expr.to_string z3exp in
        if String.equal str "this" then Z3.BitVector.mk_const_s ctx (fst this_addr) 160
        else Z3.BitVector.mk_const_s ctx str 160
      | EType (Bytes n1), EType (Bytes n2) ->
        (* bytes8 b8 = 0x00aaff00111100ff;
          bytes4(b8) => 0x00aaff00
          bytes16(b8) => 0x00aaff00111100ff0000000000000000 *)
        if n1 = n2 then z3exp
        else if n1 > n2 then Z3.BitVector.mk_extract ctx ((n1 * 8) - 1) ((n1 * 8) - (n2 * 8)) z3exp
        else if n1 < n2 then
          let zeros = Z3.BitVector.mk_numeral ctx "0" ((n2 - n1) * 8) in
          Z3.BitVector.mk_concat ctx z3exp zeros
        else assert false
      | EType (Enum _), EType (UInt n) ->
        (* enum is interpreted as unsigned 256-bit exps. *)
        if n < 256 then Z3.BitVector.mk_extract ctx (n - 1) 0 z3exp else z3exp
      | ConstInt, EType (Enum _) -> Z3.Arithmetic.Integer.mk_int2bv ctx 256 z3exp
      | EType (UInt 256), EType (Enum _) -> z3exp
      | EType (UInt n), EType (Enum _) -> Z3.BitVector.mk_zero_ext ctx (256 - n) z3exp
      | ConstString _, EType DBytes
      (* XXX: explicit conversion possible, but implicit conversion impossible *)
      | EType String, EType DBytes ->
        Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort comp_typ)
        (* XXX: explicit conversion possible, but implicit conversion impossible *)
      | EType DBytes, EType String ->
        Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort comp_typ)
      | ConstString _, EType (Bytes _) ->
        Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort comp_typ)
      | EType (UInt n1), EType (Bytes n2) ->
        (* disallowed since solc 0.5.0 *)
        (* uint n = 0xffff11aa; bytes2(n)) => 0x11aa *)
        (* uint8 n8 = 0xaf; bytes2(n8) => 0x00af *)
        if n1 = n2 * 8 then z3exp
        else if n1 > n2 * 8 then Z3.BitVector.mk_extract ctx ((n2 * 8) - 1) 0 z3exp
        else if n1 < n2 * 8 then Z3.BitVector.mk_zero_ext ctx ((n2 * 8) - n1) z3exp
        else assert false
          (* XXX: currently, bounded and unbounded arrays are handled in the same way *)
      | Array (t1, Some _), Array (t2, None) when t1 = t2 -> z3exp
      | EType (Contract _), EType (UInt _) ->
        Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort comp_typ)
      | EType Address, EType (UInt n) -> convert (EType (UInt 160)) (EType (UInt n)) z3exp
      | EType (UInt n), EType Address -> convert (EType (UInt n)) (EType (UInt 160)) z3exp
      | EType AddressPayable, EType (UInt n) -> convert (EType (UInt 160)) (EType (UInt n)) z3exp
      | EType (UInt n), EType AddressPayable -> convert (EType (UInt n)) (EType (UInt 160)) z3exp
      | EType Address, EType AddressPayable -> z3exp
      | EType AddressPayable, EType Address -> z3exp
      | (Struct _, EType (UInt n) | EType (UInt n), Struct _) when n > 256 -> z3exp
      | _ ->
        Say.pp ~lv:1 "[WARNING] [strange conversion] %a -> %a" Typ.pp my_typ Typ.pp comp_typ;
        (* for unknown cases, simply assign fresh variables *)
        Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort comp_typ)
  ;;

  let rec vformula_to_z3exp (vf : vformula) : Z3.Expr.expr =
    match vf with
    | VNot (ForAll (vars, VNot f)) ->
      let bound_vars = List.map (fun (x, t) -> vexp_to_z3exp (VVar (x, t))) vars in
      let body = vformula_to_z3exp f in
      Z3.Quantifier.expr_of_quantifier
        (Z3.Quantifier.mk_exists_const ctx bound_vars body None [] [] None None)
    | VTrue -> Z3.Boolean.mk_true ctx
    | VFalse -> Z3.Boolean.mk_false ctx
    | VNot f -> Z3.Boolean.mk_not ctx (vformula_to_z3exp f)
    | VAnd (f1, f2) -> Z3.Boolean.mk_and ctx [ vformula_to_z3exp f1; vformula_to_z3exp f2 ]
    | VOr (f1, f2) ->
      Z3.Boolean.mk_or ctx [ vformula_to_z3exp f1; vformula_to_z3exp f2 ]
      (* TODO: currently, assume ve1,ve2 are unsigned expressions. 
        should consider signed cases as well. *)
    | VBinRel (vbrel, ve1, ve2) -> (
      let z3exp1 = vexp_to_z3exp ve1 in
      let z3exp2 = vexp_to_z3exp ve2 in
      match vbrel with
      | VGeq -> Z3.BitVector.mk_uge ctx z3exp1 z3exp2
      | VGt -> Z3.BitVector.mk_ugt ctx z3exp1 z3exp2
      | VEq -> Z3.Boolean.mk_eq ctx z3exp1 z3exp2)
    | Imply (f1, f2) -> Z3.Boolean.mk_implies ctx (vformula_to_z3exp f1) (vformula_to_z3exp f2)
    | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ ->
      failwith ("z3interface: " ^ to_string_vformula vf)
    | ForAll ([ i ], VBinRel (VEq, Read (VVar a, i'), e))
      when VVar i = i' || Typ.is_mapping2 (snd a) ->
      (* todo: introduce constant arrays to formula.ml and erase this *)
      let i_sort = Typ.domain (snd a) |> typ_to_sort in
      let e = vexp_to_z3exp e in
      let a = vexp_to_z3exp (VVar a) in
      Z3.Boolean.mk_eq ctx a (Z3.Z3Array.mk_const_array ctx i_sort e)
    | ForAll ([ i; j ], VBinRel (VEq, Read (Read (VVar a, VVar i'), VVar j'), e))
      when i = i' && j = j' ->
      let i_sort = snd i |> typ_to_sort in
      let j_sort = snd j |> typ_to_sort in
      let e = vexp_to_z3exp e in
      let a = vexp_to_z3exp (VVar a) in
      Z3.Boolean.mk_eq ctx a
        (Z3.Z3Array.mk_const_array ctx i_sort (Z3.Z3Array.mk_const_array ctx j_sort e))
    | ForAll (vars, f) ->
      let bound_vars = List.map (fun (x, t) -> vexp_to_z3exp (VVar (x, t))) vars in
      let body = vformula_to_z3exp f in
      Z3.Quantifier.expr_of_quantifier
        (Z3.Quantifier.mk_forall_const ctx bound_vars body None [] [] None None)
    | Label _ -> failwith "VeriSmart.z3Functor.vformula_to_z3exp: Label"

  and vexp_to_z3exp (vexp : vexp) : Z3.Expr.expr =
    match vexp with
    | VInt n -> Z3.Arithmetic.Integer.mk_numeral_s ctx (Z.to_string n)
    | Str (s, _) -> Z3.Seq.mk_string ctx s
    | VVar (vid, typ) -> Z3.Expr.mk_const_s ctx vid (typ_to_sort typ)
    | Read (ve1, ve2) when Typ.is_bytes (get_typ_vexp ve1) -> (
      (* TODO: move to semantics.ml *)
      let z3exp1 = vexp_to_z3exp ve1 in
      let size = Typ.get_bytes_size (get_typ_vexp ve1) in
      match ve2 with
      | VInt n ->
        let i = Z.to_int n in
        Z3.BitVector.mk_extract ctx ((size * 8) - (i * 8) - 1) ((size * 8) - (i * 8) - 8) z3exp1
      | _ -> Z3.Expr.mk_fresh_const ctx (freshvar ()) (typ_to_sort (get_typ_vexp vexp)))
    | Read (ve1, ve2) ->
      let z3exp1 = vexp_to_z3exp ve1 in
      let z3exp2 = vexp_to_z3exp ve2 in
      Z3.Z3Array.mk_select ctx z3exp1 z3exp2
    | Write (ve1, ve2, ve3) ->
      let z3exp1 = vexp_to_z3exp ve1 in
      let z3exp2 = vexp_to_z3exp ve2 in
      let z3exp3 = vexp_to_z3exp ve3 in
      Z3.Z3Array.mk_store ctx z3exp1 z3exp2 z3exp3
    | VBinOp (vbop, ve1, ve2, _typ) ->
      let z3exp1 = vexp_to_z3exp ve1 in
      let z3exp2 = vexp_to_z3exp ve2 in
      begin
        match vbop with
        | VAdd -> Z3.BitVector.mk_add ctx z3exp1 z3exp2
        | VSub -> Z3.BitVector.mk_sub ctx z3exp1 z3exp2
        | VMul -> Z3.BitVector.mk_mul ctx z3exp1 z3exp2
        | VDiv ->
          Z3.BitVector.mk_udiv ctx z3exp1 z3exp2 (* TODO: currently, assume unsigned expressions *)
        | VMod ->
          Z3.BitVector.mk_urem ctx z3exp1 z3exp2 (* TODO: currently, assume unsigned expressions *)
        | VPower -> raise CannotDealPowerExprs
        | VShiftL -> Z3.BitVector.mk_shl ctx z3exp1 z3exp2
        | VShiftR -> Z3.BitVector.mk_lshr ctx z3exp1 z3exp2
        | VBXor -> Z3.BitVector.mk_xor ctx z3exp1 z3exp2
        | VBAnd -> Z3.BitVector.mk_and ctx z3exp1 z3exp2
        | VBOr -> Z3.BitVector.mk_or ctx z3exp1 z3exp2
        | VBVConcat -> Z3.BitVector.mk_concat ctx z3exp1 z3exp2
      end
    | VUnOp (vuop, ve, _typ) ->
      let z3exp = vexp_to_z3exp ve in
      begin
        match vuop with
        | VNeg -> Z3.BitVector.mk_neg ctx z3exp
        | VBNot -> Z3.BitVector.mk_not ctx z3exp
      end
    | VCast (typ, ve) ->
      let ve_typ = get_typ_vexp ve in
      let ve' = convert ve_typ typ (vexp_to_z3exp ve) in
      ve'
    | VCond vf -> vformula_to_z3exp vf
    | Ite (ve1, ve2, ve3) ->
      Z3.Boolean.mk_ite ctx (vexp_to_z3exp ve1) (vexp_to_z3exp ve2) (vexp_to_z3exp ve3)
    | Uninterp (fname, args, t) ->
      let sorts = List.map (fun e -> e |> get_typ_vexp |> typ_to_sort) args in
      let func = Z3.FuncDecl.mk_func_decl_s ctx fname sorts (typ_to_sort t) in
      let z3args = List.map vexp_to_z3exp args in
      Z3.FuncDecl.apply func z3args
  ;;

  let common_typ : vexp -> vexp -> Typ.t =
   fun e1 e2 ->
    let t1, t2 = (get_typ_vexp e1, get_typ_vexp e2) in
    if t1 = t2 then t1
    else
      try
        begin
          match (t1, t2) with
          | ConstInt, EType (UInt n) ->
            let n' = bit_unsigned_of_int (get_bigint_v e1) 8 in
            EType (UInt (max n n'))
          | EType (UInt n), ConstInt ->
            let n' = bit_unsigned_of_int (get_bigint_v e2) 8 in
            EType (UInt (max n n'))
          | ConstInt, EType (SInt n) ->
            let n' = bit_signed_of_int (get_bigint_v e1) 8 in
            EType (SInt (max n n'))
          | EType (SInt n), ConstInt ->
            let n' = bit_signed_of_int (get_bigint_v e2) 8 in
            EType (SInt (max n n'))
          | _ -> Typ.preceding t1 t2
        end
      with Invalid_argument _ -> Typ.preceding t1 t2
  ;;

  let rec cast_vf vf =
    match vf with
    | VTrue | VFalse -> vf
    | VNot f -> VNot (cast_vf f)
    | VAnd (f1, f2) -> VAnd (cast_vf f1, cast_vf f2)
    | VOr (f1, f2) -> VOr (cast_vf f1, cast_vf f2)
    | VBinRel (vbrel, e1, e2) ->
      let t1, t2 = (get_typ_vexp e1, get_typ_vexp e2) in
      let ctyp =
        try common_typ e1 e2
        with err ->
          Pp.epr "%a@.%a@." pp_vexp e1 pp_vexp e2;
          raise err
      in
      let e1' = if t1 = ctyp then cast_ve e1 else VCast (ctyp, cast_ve e1) in
      let e2' = if t2 = ctyp then cast_ve e2 else VCast (ctyp, cast_ve e2) in
      VBinRel (vbrel, e1', e2')
    | Imply (f1, f2) -> Imply (cast_vf f1, cast_vf f2)
    | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ -> invalid_arg ""
    | ForAll (vars, f) -> ForAll (vars, cast_vf f)
    | Label _ -> invalid_arg "label should be erased before asking to the solver"

  and cast_ve ve =
    match ve with
    | VInt _ | Str _ | VVar _ -> ve
    | Read (e1, e2) ->
      let expected_idx_typ = Typ.domain (get_typ_vexp e1) in
      let idx_typ = get_typ_vexp e2 in
      let e1' = cast_ve e1 in
      let e2' =
        if expected_idx_typ = idx_typ then cast_ve e2 else VCast (expected_idx_typ, cast_ve e2)
      in
      Read (e1', e2')
    | Write (e1, e2, e3) ->
      let expected_range_typ = Typ.range (get_typ_vexp e1) in
      let range_typ = get_typ_vexp e3 in
      let expected_idx_typ = Typ.domain (get_typ_vexp e1) in
      let idx_typ = get_typ_vexp e2 in
      let e1' = cast_ve e1 in
      let e2' =
        if expected_idx_typ = idx_typ then cast_ve e2 else VCast (expected_idx_typ, cast_ve e2)
      in
      let e3' =
        if expected_range_typ = range_typ then cast_ve e3 else VCast (expected_range_typ, cast_ve e3)
      in
      Write (e1', e2', e3')
    | VBinOp (VBVConcat, e1, e2, t) -> VBinOp (VBVConcat, cast_ve e1, cast_ve e2, t)
    | VBinOp (vbop, e1, e2, t) when vbop = VShiftL || vbop = VShiftR ->
      (* Seperate from the below case, when common types cannot be computed in general. *)
      (* E.g., b << n where 'b': bytes32 and 'n': uint 256 *)
      let t1 = get_typ_vexp e1 in
      let t2 = get_typ_vexp e2 in
      (* Solidity doc says The right operand must be 'unsigned' types. *)
      (* However, 'n1 << n2' can be compiled with solc 0.6.3 (n1:uint, n2:int). *)
      (* For the right operand, 'positive' values seems enough. *)
      (* Also, the invariant 't=t1' does not hold. 
        Different from the official Solidity document, '3<<n' (where 'n' is uint type) 
        has 'uint' type. *)
      (* let _ = assert (t = t1) in *)
      (* In conclusion, simply obey types produced by solc. *)
      let e1' = if t = t1 then cast_ve e1 else VCast (t, cast_ve e1) in
      let e2' =
        if t = t2 then cast_ve e2 (* e.g., "i*uint(8)" in "bytes32 << i*8" *)
        else VCast (t, cast_ve e1)
        (* line 417 of 2018-13165.sol *)
      in
      VBinOp (vbop, e1', e2', t)
    | VBinOp (vbop, e1, e2, t) ->
      let t1 = get_typ_vexp e1 in
      let t2 = get_typ_vexp e2 in
      let e1' = if t1 = t then cast_ve e1 else VCast (t, cast_ve e1) in
      let e2' = if t2 = t then cast_ve e2 else VCast (t, cast_ve e2) in
      VBinOp (vbop, e1', e2', t)
    | VUnOp (VBNot, VInt n, ConstInt) ->
      (* the syntax is allowed in solc, but needs casting. *)
      if Z.(geq n zero) then
        let bit = bit_unsigned_of_int (get_bigint_v (VInt n)) 8 in
        VUnOp (VBNot, VCast (EType (UInt bit), VInt n), EType (UInt bit))
      else
        let bit = bit_signed_of_int (get_bigint_v (VInt n)) 8 in
        VUnOp (VBNot, VCast (EType (SInt bit), VInt n), EType (SInt bit))
    | VUnOp (vuop, e, t) -> VUnOp (vuop, cast_ve e, t)
    | VCast (t, e) -> VCast (t, cast_ve e)
    | VCond f -> VCond (cast_vf f)
    | Ite (e1, e2, e3) -> Ite (cast_ve e1, cast_ve e2, cast_ve e3)
    | Uninterp (fname, args, t) -> Uninterp (fname, List.map cast_ve args, t)
  ;;

  let chk_basic vf =
    let vf =
      try cast_vf vf
      with e ->
        Pp.pr "%a\n%!" pp_vformula vf;
        raise e
    in
    let z3formula = vformula_to_z3exp vf in
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver [ z3formula ];
    let ret =
      (* full_major (); *)
      match Z3.Solver.check solver [] with
      | UNSATISFIABLE -> (Z3.Solver.UNSATISFIABLE, None)
      | SATISFIABLE -> (SATISFIABLE, Z3.Solver.get_model solver)
      | UNKNOWN ->
        let reason = Z3.Solver.get_reason_unknown solver in
        if reason = "interrupted from keyboard" then Options.kill_switch := true;
        (UNKNOWN, None)
    in
    ret
  ;;

  let chk ?(reason = Solver_call_reason.VC) vf =
    let time = Sys.time () in
    let stat, model = chk_basic vf in
    let time_spent = Sys.time () -. time in
    time_statistics := Map.modify reason (( +. ) time_spent) !time_statistics;
    (stat, model, time_spent)
  ;;
end

module Main = Make (struct
  let id = 1
end)

module Sub = Make (struct
  let id = 2
end)

module SubSub = Make (struct
  let id = 3
end)
