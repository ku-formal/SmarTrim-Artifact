open! Frontend
open Frontend.Lang
open Vlang
open Vocab
open Vocab.Batteries
open Options
open FuncMap

(**************************)
(**************************)
(**** Helper functions ****)
(**************************)
(**************************)

open struct
  module Rename_counter = Counter.M ()
end

let fresh_rid () = Rename_counter.gen ()
let reset_rid () = Rename_counter.reset ()

let rename (vexp : vexp) : vexp =
  match vexp with
  | VVar (vid, typ) ->
    let vid' = vid ^ "(#" ^ string_of_int (fresh_rid ()) ^ ")" in
    VVar (vid', typ)
  | _ -> failwith "rename"
;;

let is_renamed (x : string) : bool = String.exists x "#"

(* XXX: should be changed in accordance with
 * XXX: 1) 'convert_lv' in semantics.ml
 * XXX: 2) 'get_target' in semantics.ml
 * XXX: 3) 'get_defs_lv' in funcDefVar.ml
 * XXX: 4) 'loc_of' in itvSem.ml *)
let rec get_target (lv : lv) : vexp =
  match lv with
  | Var (id, vinfo) -> VVar (id, vinfo.vtyp)
  | MemberAccess (e, "length", info, _) -> VVar ("@L", Mapping2 (get_type_exp e, info.vtyp))
  | MemberAccess (e, "balance", info, _) ->
    assert (info.vtyp = EType (UInt 256));
    assert (Typ.is_address_kind (get_type_exp e));
    VVar ("@B", Mapping (Address, info.vtyp))
  | MemberAccess (e, x, xinfo, _) -> VVar (x, Mapping2 (get_type_exp e, xinfo.vtyp))
  | IndexAccess (Lv lv, _, _) -> get_target lv
  | _ -> Pp.failwithf "semantics (get_target) : %s" (to_string_lv lv)
;;

let rec recur (ve : vexp) : vexp =
  match ve with
  | Write (VVar _, _, _) -> ve
  | Write (Read (ve1, ve2), _idx, _e) -> recur (Write (ve1, ve2, ve))
  | _ -> failwith ("recur : " ^ to_string_vexp ve)
;;

(***********************)
(***********************)
(**** SP Generation ****)
(***********************)
(***********************)

let rec convert_aexp (exp : exp) : vexp =
  let c = convert_aexp in
  match exp with
  | V (Int n) -> VInt n
  | V (Bool true) -> VCond VTrue
  | V (Bool false) -> VCond VFalse
  | V (Frac _) -> failwith "convert_aexp : rational literal"
  | Str s -> Str (s, EType String)
  | Lv lv ->
    let s = to_string_lv lv in
    if s = "this" then VVar this_addr
    else if List.mem s Lang.keyword_vars then VVar (s, get_type_lv lv)
    else convert_lv lv
  | Cast (EType Address, Lv (Var ("this", _))) ->
    VVar this_addr (* specifics are handled in z3Interface. *)
  | Cast (typ, e) -> VCast (typ, c e)
  | Ite (i, t, e, _) -> Ite (c i, c t, c e)
  | UnOp (uop, e, typ) -> begin
    match uop with
    | Pos -> c e
    | Neg -> VUnOp (VNeg, c e, typ)
    | BNot -> VUnOp (VBNot, c e, typ)
    | LNot -> VCond (VNot (convert_bexp e))
  end
  | BinOp (bop, e1, e2, _einfo) -> begin
    let typ = get_type_exp exp in
    match bop with
    | Add -> VBinOp (VAdd, c e1, c e2, typ)
    | Sub -> VBinOp (VSub, c e1, c e2, typ)
    | Mul -> VBinOp (VMul, c e1, c e2, typ)
    | Div -> VBinOp (VDiv, c e1, c e2, typ)
    | Mod -> VBinOp (VMod, c e1, c e2, typ)
    | Exponent -> VBinOp (VPower, c e1, c e2, typ)
    | ShiftL -> VBinOp (VShiftL, c e1, c e2, typ)
    | ShiftR -> VBinOp (VShiftR, c e1, c e2, typ)
    | BAnd -> VBinOp (VBAnd, c e1, c e2, typ)
    | BOr -> VBinOp (VBOr, c e1, c e2, typ)
    | BXor -> VBinOp (VBXor, c e1, c e2, typ)
    | GEq -> VCond (VBinRel (VGeq, c e1, c e2))
    | Gt -> VCond (VBinRel (VGt, c e1, c e2))
    | LEq -> VCond (VBinRel (VGeq, c e2, c e1))
    | Lt -> VCond (VBinRel (VGt, c e2, c e1))
    | Eq -> VCond (VBinRel (VEq, c e1, c e2))
    | NEq -> VCond (VNot (VBinRel (VEq, c e1, c e2)))
    | LAnd -> VCond (VAnd (convert_bexp e1, convert_bexp e2))
    | LOr -> VCond (VOr (convert_bexp e1, convert_bexp e2))
  end
  | TypeInfo (typ, x, _einfo) -> (
    match (typ, x) with
    | EType (Contract cid), "name" -> VVar ("@" ^ cid, EType String)
    | EType (Contract cid), "interfaceId" -> VVar ("@" ^ cid ^ ".interfaceId", get_type_exp exp)
    | EType (UInt n), "max" -> VCast (EType (UInt 256), VInt (max_of_n_bits n))
    | EType (UInt n), "min" -> VCast (EType (UInt n), VInt Z.zero)
    | _ -> raise NotImplemented)
  | _ -> failwith "convert_aexp : tmp expressions should not exist"

and convert_lv lv =
  let typ = get_type_lv lv in
  match lv with
  | Var (x, _) -> VVar (x, typ)
  | IndexAccess (e1, Some e2, _t) when Typ.is_dbytes (get_type_exp e1) ->
    let lmap_typ : Typ.t = Mapping2 (get_type_exp e1, EType (UInt 256)) in
    let size = Read (VVar (length_map, lmap_typ), convert_aexp e1) in
    let idx =
      VBinOp
        (VSub, VBinOp (VSub, size, VInt Z.one, EType (UInt 256)), convert_aexp e2, EType (UInt 256))
    in
    Read (convert_aexp e1, idx)
  | IndexAccess (e1, Some e2, _t) -> Read (convert_aexp e1, convert_aexp e2)
  | IndexAccess (_, None, _) -> failwith "convert_lv - IndexAccess"
  | MemberAccess (e, "balance", _, _) -> Read (VVar eth_map, convert_aexp e)
  | MemberAccess (e, "length", _, _) when Typ.is_array (get_type_exp e) ->
    Read (VVar (length_map, Mapping2 (get_type_exp e, typ)), convert_aexp e)
  | MemberAccess (_, "selector", _, _) -> VVar (to_string_lv lv, typ) (* typ: bytes4 *)
  | MemberAccess (_, x, _, _) when Typ.is_enum typ && String.exists x "__idx" ->
    let _, idx = String.split x ~by:"__idx" in
    VCast (typ, VInt (Z.of_int (int_of_string idx)))
  | MemberAccess (e, x, xinfo, _) ->
    Read (VVar (x, Mapping2 (get_type_exp e, xinfo.vtyp)), convert_aexp e)
  | Tuple _ ->
    (* handled in assignment level *)
    Pp.failwithf "convert_lv - Tuple : %s" (to_string_lv lv)

and convert_bexp (exp : exp) : vformula =
  let convert_bexp_lv lv =
    assert (Typ.is_bool (get_type_lv lv));
    VBinRel (VEq, convert_lv lv, VCond VTrue)
  in
  match exp with
  | V (Bool true) -> VTrue
  | V (Bool false) -> VFalse
  | Lv lv -> convert_bexp_lv lv
  | BinOp (bop, e1, e2, _einfo) -> (
    assert ((not (Typ.is_dbytes (get_type_exp e1))) && not (Typ.is_dbytes (get_type_exp e1)));
    match bop with
    | GEq -> VBinRel (VGeq, convert_aexp e1, convert_aexp e2)
    | Gt -> VBinRel (VGt, convert_aexp e1, convert_aexp e2)
    | LEq -> VBinRel (VGeq, convert_aexp e2, convert_aexp e1)
    | Lt -> VBinRel (VGt, convert_aexp e2, convert_aexp e1)
    | Eq -> VBinRel (VEq, convert_aexp e1, convert_aexp e2)
    | NEq -> VNot (VBinRel (VEq, convert_aexp e1, convert_aexp e2))
    | LAnd -> VAnd (convert_bexp e1, convert_bexp e2)
    | LOr -> VOr (convert_bexp e1, convert_bexp e2)
    | _ -> failwith ("convert_bexp1 : " ^ to_string_exp exp))
  | UnOp (uop, e, _typ) -> (
    let _ = assert (not (Typ.is_dbytes (get_type_exp e))) in
    match uop with
    | LNot -> VNot (convert_bexp e)
    | _ -> failwith ("convert_bexp2 : " ^ to_string_exp exp))
  | _ -> failwith ("convert_bexp3 : " ^ to_string_exp exp)
;;

let rewrite_q target replacement (q : Query.t) : Query.t =
  { q with vc2 = (match q.vc2 with Imply _ -> q.vc2 | _ -> rewrite_vf q.vc2 target replacement) }
;;

(* {q with vc2 = rewrite_vf q.vc2 target replacement} *)

let rec assign (lv, e) (vf, qs) =
  let lv_typ = get_type_lv lv in
  match (lv, e) with
  (* TODO: This is an array initialization statment. may need to move to other places *)
  | _, Lv (Tuple (eops, _)) when Typ.is_array lv_typ ->
    List.fold_lefti
      (fun (acc_vf, acc_qs) i eop ->
        match eop with
        | Some e ->
          let t = Typ.get_array_elem lv_typ in
          (* type of "lv[i]" *)
          assign (IndexAccess (Lv lv, Some (V (Int (Z.of_int i))), t), e) (acc_vf, acc_qs)
        | None -> (acc_vf, acc_qs))
      (vf, qs) eops
  | Var _v, e ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve' = rewrite_ve (convert_aexp e) target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    (VAnd (vf', Label (Assign_info `Assign, VBinRel (VEq, target, ve'))), qs')
  | IndexAccess (arr, Some idx, _t), e ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve = recur (Write (convert_aexp arr, convert_aexp idx, convert_aexp e)) in
    let ve' = rewrite_ve ve target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    (VAnd (vf', Label (Assign_info `Assign, VBinRel (VEq, target, ve'))), qs')
  | MemberAccess (e1, _, _, _), e2 ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve = Write (target, convert_aexp e1, convert_aexp e2) in
    let ve' = rewrite_ve ve target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    (VAnd (vf', Label (Assign_info `Assign, VBinRel (VEq, target, ve'))), qs')
  | Tuple (eops1, _), Lv (Tuple (eops2, _)) ->
    (* (a,b) := (1,2) *)
    List.fold_left2
      (fun (acc_vf, acc_qs) eop1 eop2 ->
        match (eop1, eop2) with
        | Some (Lv lv'), Some e' -> assign (lv', e') (acc_vf, acc_qs)
        | None, Some _e' -> (acc_vf, acc_qs)
        | _ -> failwith "invalid tuple assignment1")
      (vf, qs) eops1 eops2
  | Tuple _, _ -> failwith "invalid tuple assignment2"
  | IndexAccess (_, None, _), _ -> failwith "invalid IndexAccess assignment"
;;

let assign_ve ?(force_assume = false) (lv, ve) (vf, qs) =
  match (lv, ve) with
  | Var _v, ve ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve' = rewrite_ve ve target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    let assign = if force_assume then `Assume else `Assign in
    (VAnd (vf', Label (Assign_info assign, VBinRel (VEq, target, ve'))), qs')
  | IndexAccess (arr, Some idx, _t), ve ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve = recur (Write (convert_aexp arr, convert_aexp idx, ve)) in
    let ve' = rewrite_ve ve target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    let assign = if force_assume then `Assume else `Assign in
    (VAnd (vf', Label (Assign_info assign, VBinRel (VEq, target, ve'))), qs')
  | MemberAccess (e1, _, _, _), ve ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let ve = Write (target, convert_aexp e1, ve) in
    let ve' = rewrite_ve ve target replacement in
    let qs' = List.map (rewrite_q target replacement) qs in
    let assign = if force_assume then `Assume else `Assign in
    (VAnd (vf', Label (Assign_info assign, VBinRel (VEq, target, ve'))), qs')
  | Tuple _, _ -> failwith "invalid tuple assignment2"
  | IndexAccess (_, None, _), _ -> failwith "invalid IndexAccess assignment"
;;

let bcnt = ref 0 (* bound variable i counter *)

let new_bound_var typ =
  incr bcnt;
  ("@i" ^ string_of_int !bcnt, typ)
;;

(* generate constraint for 'Decl lv' *)
(* bvars : accumulated bound variables; accumulated whenever 'mapping' type is encountered. *)
let rec decl (smap : StructMap.t) (bvars : Var.t list) (vexp : vexp) : vformula =
  let typ = get_typ_vexp vexp in
  match typ with
  | ConstInt | ConstString _ | ConstReal -> assert false
  | EType etyp ->
    let vf =
      match etyp with
      | Address | AddressPayable -> VBinRel (VEq, vexp, VInt Z.zero)
      | Contract cid -> VBinRel (VEq, vexp, VCast (EType (Contract cid), VInt Z.zero))
      | Enum _ -> VBinRel (VEq, vexp, VCast (typ, VInt Z.zero))
      | Bool -> VBinRel (VEq, vexp, VCond VFalse)
      | String -> VBinRel (VEq, vexp, Str ("", EType String))
      | UInt _ -> VBinRel (VEq, vexp, VInt Z.zero)
      | SInt _ -> VBinRel (VEq, vexp, VInt Z.zero)
      | Bytes _ -> VBinRel (VEq, vexp, VInt Z.zero)
      | DBytes ->
        (* TODO *)
        VTrue
    in
    if List.length bvars = 0 then vf else ForAll (bvars, vf)
  | Mapping (etyp', _typ') ->
    let i = new_bound_var (Typ.EType etyp') in
    let bvars' = bvars @ [ i ] in
    let vexp' = Read (vexp, VVar i) in
    decl smap bvars' vexp'
    (* XXX: multi-dimensional array? *)
  | Array (_t, sizeop) ->
    let i = new_bound_var (Typ.EType (UInt 256)) in
    let bvars' = bvars @ [ i ] in
    let vexp' = Read (vexp, VVar i) in
    let init_vf = decl smap bvars' vexp' in
    let length_vf =
      match sizeop with
      | None ->
        (* dynamic array => @L[arr] = 0 *)
        VBinRel (VEq, Read (VVar (length_map, Mapping2 (typ, EType (UInt 256))), vexp), VInt Z.zero)
      | Some n ->
        (* static array with size 'n' => @L[arr] = n *)
        VBinRel
          (VEq, Read (VVar (length_map, Mapping2 (typ, EType (UInt 256))), vexp), VInt (Z.of_int n))
    in
    VAnd (init_vf, length_vf)
  | Struct lst ->
    let members = StructMap.find (Typ.get_name_userdef typ) smap in
    let members = List.map Var2.(fun m -> (m.id, m.info.vtyp)) members in
    List.fold_left
      (fun acc m ->
        let ve = Read (VVar (fst m, Mapping2 (Struct lst, snd m)), vexp) in
        (* s.m => m'[s] *)
        let new_f = decl smap bvars ve in
        VAnd (acc, new_f))
      VTrue members
  | Mapping2 _ -> assert false
  | TupleType _ | FuncType _ -> assert false
  | Void ->
    print_endline (to_string_vexp vexp ^ " : " ^ Typ.to_string (get_typ_vexp vexp));
    assert false
;;

let convert_decl (struct_map : StructMap.t) lv =
  let ve = convert_lv lv in
  let vf = decl struct_map [] ve in
  let _ = bcnt := 0 in
  vf
;;

let handle_init_funcs (lvop, f, _args) (vf, qs) =
  match lvop with
  | None -> failwith ("handle_init_funcs : " ^ f)
  | Some _ -> (
    match f with
    | "array_init" -> (vf, qs)
    | "dbytes_init" -> (vf, qs)
    | "string_init" -> (vf, qs)
    | "contract_init" -> (vf, qs)
    | "struct_init" ->
      failwith "struct_init : semantics.ml" (* must be handled in preprocessing step. *)
    | _ -> failwith ("handle_init_funcs : " ^ f))
;;

let handle_built_in_funcs (lvop, f, args) (vf, qs) =
  match lvop with
  | None -> begin
    match f with
    | "revert" -> (VFalse, qs)
    | "selfdestruct" | "suicide" -> (VFalse, qs)
    | "delete" -> (vf, qs)
    | _ -> Pp.failwithf "handle_built_in_funcs: %s" f
  end
  | Some lv ->
    (* invariant: lv is in 'Var' pattern *)
    begin
      match f with
      | "keccak256" | "sha3" ->
        if !mode <> Exploit then (vf, qs)
        else begin
          let args = List.map convert_aexp args in
          match Hash_model.add_assumption_and_get_output_exp vf args with
          | Some (vf, output) ->
            (* assign *)
            assign_ve ~force_assume:true (lv, output) (vf, qs)
          | None -> (vf, qs)
        end
      | "sha256" | "ripemd160" -> (vf, qs)
      | "ecrecover" ->
        (* recover address *)
        if !mode = Exploit then (VFalse, qs) else (vf, qs)
      | "addmod" -> (vf, qs)
      | "blockhash" -> (vf, qs) (* hash of the given block *)
      | "gasleft" -> (vf, qs) (* todo: currently we give up and pass, this is temporal *)
      | _ -> failwith ("handle_built_in_funcs: " ^ f)
    end
;;

let handle_address_built_in _loc _rcv (_lvop, f, _args) (_ethop, _gasop) (vf, qs) =
  match f with
  | "transfer" -> (vf, qs)
  | "send" -> (vf, qs)
  | "call" -> (vf, qs)
  | "balance" -> (vf, qs)
  | "delegatecall" | "staticcall" -> (vf, qs)
  | _ -> failwith ("handle_address_built_in : " ^ f)
;;

let handle_array_built_in smap (_lvop, exp, args) fname loc (vf, qs) =
  match exp with
  | Lv (MemberAccess (arr, "push", _, _)) ->
    (* tmp := @L[arr]; arr[tmp] := v; @L[arr] := tmp + 1 *)
    (* Without the first stmt, we get unsound results. *)
    let len_map_typ : Typ.t = Mapping2 (get_type_exp arr, EType (UInt 256)) in
    let len_arr =
      IndexAccess (Lv (Var (length_map, mk_vinfo ~typ:len_map_typ ())), Some arr, EType (UInt 256))
    in
    (* @L[arr] *)
    let tmp = Var (fst (gen_newsym (Typ.EType (UInt 256))), mk_vinfo ~typ:(EType (UInt 256)) ()) in
    let arr_tmp = IndexAccess (arr, Some (Lv tmp), Typ.range (get_type_exp arr)) in
    (* arr[tmp] *)
    let vf1, qs1 = assign (tmp, Lv len_arr) (vf, qs) in
    (* first stmt *)
    let snd_stmt =
      match List.length args with
      | 0 ->
        (* 'arr.push()' is possible for solc >= 0.6.0 *)
        Stmt.Decl arr_tmp
        (* initialize (arr[tmp]) : even 'mapping' type can be initialized when 'arr' is an array of mapping. *)
      | 1 -> Assign (arr_tmp, List.hd args, loc)
      | _ -> assert false
    in
    let vf2, qs2 =
      (* snd stmt *)
      match snd_stmt with
      | Decl lv ->
        let vf' = convert_decl smap lv in
        let vf'' = VAnd (vf1, vf') in
        (vf'', qs1)
      | Assign (lv, e, _loc) -> assign (lv, e) (vf1, qs1)
      | _ -> assert false
    in
    let vf3, qs3 =
      assign (len_arr, BinOp (Add, Lv tmp, V (Int Z.one), mk_einfo (EType (UInt 256)))) (vf2, qs2)
    in
    (* third stmt *)
    (vf3, qs3)
  | Lv (MemberAccess (_e, "pop", _, _)) -> (vf, qs)
  | _ -> failwith ("handle_array_built_in : " ^ fname)
;;

let handle_abi fname (vf, qs) =
  match fname with
  | "decode" -> (vf, qs)
  | "encode" -> (vf, qs)
  | "encodePacked" -> (vf, qs)
  | "encodeWithSignature" -> (vf, qs)
  | "encodeWithSelector" -> (vf, qs)
  | _ -> raise (Failure ("handle_abi : " ^ fname))
;;

let handle_undefs (global : Global.t) (lvop, exp, args) (ethop, gasop) loc (vf, qs) =
  match exp with
  | Lv (Var (f, _)) when List.mem f built_in_funcs -> handle_built_in_funcs (lvop, f, args) (vf, qs)
  | Lv (Var (f, _)) when List.mem f init_funcs -> handle_init_funcs (lvop, f, args) (vf, qs)
  | Lv (MemberAccess (e, f, _, _)) when Typ.is_address_kind (get_type_exp e) ->
    handle_address_built_in loc e (lvop, f, args) (ethop, gasop) (vf, qs)
  | Lv (MemberAccess (e, f, _, _)) when Typ.is_array (get_type_exp e) ->
    handle_array_built_in global.struct_map (lvop, exp, args) f loc (vf, qs)
  | Lv (MemberAccess (Lv (Var ("abi", _)), f, _, _)) -> handle_abi f (vf, qs)
  | Lv (MemberAccess (Lv (Var ("block", _)), "blockhash", _, _)) -> (vf, qs)
  | _ ->
    if !debug = "undeflib" then
      prerr_endline
        ("Warning - undefined functions encountered : " ^ to_string_exp exp ^ ", line "
       ^ string_of_int loc.line);
    (vf, qs)
;;

let handle_object_call _global _caller (lvop, _e, _args) vf =
  match lvop with
  | None -> vf
  | Some lv ->
    let def = Set.to_list (FuncDefUse.get_def_set_lv lv) in
    weaken_vf2 vf def
;;

let convert_stmt (global : Global.t) curf (stmt : Stmt.t) (vf, qs) : vformula * Query.t list =
  let ftyp_params = List.filter (fun v -> Typ.is_func (snd v)) (Func.param_vars curf) in
  match stmt with
  | Assign (lv, e, _) -> assign (lv, e) (vf, qs)
  | Decl lv ->
    let target = get_target lv in
    let replacement = rename target in
    let vf' = rewrite_vf vf target replacement in
    let new_vf = convert_decl global.struct_map lv in
    let vf'' = VAnd (vf', new_vf) in
    let qs' = List.map (rewrite_q target replacement) qs in
    (vf'', qs')
  | Skip -> (vf, qs)
  | Return (Some e, _) ->
    (* ret_params <- e *)
    let ret_params = curf.ret_params in
    let lv = try params_to_lv ret_params with _ -> assert false in
    assign (lv, e) (vf, qs)
  | Return (None, _) -> (vf, qs)
  | Revert -> (VFalse, qs)
  | Assume (e, _) -> (VAnd (vf, Label (Assign_info `Assume, convert_bexp e)), qs)
  | Assert _ ->
    (vf, qs) (* effects will be reflected by subsequent statements generated in translation step. *)
  | Assembly (lst, _) ->
    let vars = List.map fst lst in
    (List.fold_left weaken_vf vf vars, qs)
  | Call (lvop, Lv (Var (v, vinfo)), _args, _ethop, _gasop, _loc)
    when List.mem (v, vinfo.vtyp) ftyp_params -> (
    (* function pointers *)
    match lvop with
    | None -> (vf, qs)
    | Some lv ->
      let def = Set.to_list (FuncDefUse.get_def_set_lv lv) in
      (weaken_vf2 vf def, qs))
  | Call (lvop, e, args, ethop, gasop, loc) when is_undef_call global.fmap stmt ->
    handle_undefs global (lvop, e, args) (ethop, gasop) loc (vf, qs)
  | Call _ when is_internal_call global.fmap global.cnames stmt -> assert false
  | Extcall _ -> assert false (* unreachable *)
  | Call (lvop, e, args, _, _, _) -> (handle_object_call global curf (lvop, e, args) vf, qs)
  | Label _ -> (vf, qs)
  | If _ | Seq _ | While _ | Break | Continue | Placeholder | Unchecked _ ->
    Pp.failwithf "convert_stmt : %s" (to_string_stmt stmt)
;;
