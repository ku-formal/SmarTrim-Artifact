(** Bit reduction. Moved from z3interface.ml, since currently unused in smartrim. *)

open! Frontend
open Formula

(****************************)
(*** Reestrict bit number ***)
(****************************)

let rec change_typ bit (typ : Typ.t) : Typ.t =
  match typ with
  | ConstInt | ConstString _ | ConstReal -> typ
  | EType et -> EType (change_etyp bit et)
  | Mapping (et, t) -> Mapping (change_etyp bit et, change_typ bit t)
  | Mapping2 (t1, t2) -> Mapping2 (change_typ bit t1, change_typ bit t2)
  | Array (t, _op) ->
    Mapping (change_etyp bit (UInt 256), change_typ bit t) (* Array (change_typ bit t, op) *)
  | Struct _ -> typ
  | TupleType lst -> TupleType (List.map (change_typ bit) lst)
  | FuncType _ -> assert false
  | Void -> typ

and change_etyp : int -> Typ.e -> Typ.e =
 fun bit etyp ->
  match etyp with
  | Contract _ | Enum _ | Address | AddressPayable | Bool | String -> etyp
  | UInt n -> if n = 256 then UInt bit else UInt n
  | SInt n -> if n = 256 then SInt bit else SInt n
  | Bytes _n -> etyp
  | DBytes -> etyp
;;

(* bit restriction *)
let rec rest_vf bit (vf : vformula) : vformula =
  match vf with
  | VTrue | VFalse -> vf
  | VNot f -> VNot (rest_vf bit f)
  | VAnd (f1, f2) -> VAnd (rest_vf bit f1, rest_vf bit f2)
  | VOr (f1, f2) -> VOr (rest_vf bit f1, rest_vf bit f2)
  | VBinRel (rel, e1, e2) -> VBinRel (rel, rest_ve bit e1, rest_ve bit e2)
  | Imply (f1, f2) -> Imply (rest_vf bit f1, rest_vf bit f2)
  | SigmaEqual ((x, t), e) -> SigmaEqual ((x, change_typ bit t), rest_ve bit e)
  | NoOverflow (x, t) -> NoOverflow (x, change_typ bit t)
  | UntrustSum ((x1, t1), (x2, t2)) -> UntrustSum ((x1, change_typ bit t1), (x2, change_typ bit t2))
  | UntrustSum2 ((x1, t1), (x2, t2), (x3, t3)) ->
    UntrustSum2 ((x1, change_typ bit t1), (x2, change_typ bit t2), (x3, change_typ bit t3))
  | ForAll (lst, f) ->
    let lst' = List.map (fun (x, t) -> (x, change_typ bit t)) lst in
    ForAll (lst', rest_vf bit f)
  | Label (l, f) -> Label (l, rest_vf bit f)

and rest_ve bit (ve : vexp) : vexp =
  match ve with
  | VInt _ | Str _ -> ve
  | VVar (x, t) -> VVar (x, change_typ bit t)
  | Read (e1, e2) -> Read (rest_ve bit e1, rest_ve bit e2)
  | Write (e1, e2, e3) -> Write (rest_ve bit e1, rest_ve bit e2, rest_ve bit e3)
  | VBinOp (bop, e1, e2, t) -> VBinOp (bop, rest_ve bit e1, rest_ve bit e2, change_typ bit t)
  | VUnOp (uop, e, t) -> VUnOp (uop, rest_ve bit e, change_typ bit t)
  | VCast (t, e) -> VCast (change_typ bit t, rest_ve bit e)
  | VCond f -> VCond (rest_vf bit f)
  | Ite (e1, e2, e3) -> Ite (rest_ve bit e1, rest_ve bit e2, rest_ve bit e3)
  | Uninterp (fname, args, t) -> Uninterp (fname, List.map (rest_ve bit) args, change_typ bit t)
;;

let reduct_bit : int -> vformula -> vformula = fun bit vf -> rest_vf bit vf
