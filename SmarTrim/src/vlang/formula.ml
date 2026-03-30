open! Frontend

(*******************************)
(*******************************)
(**** Verification Language ****)
(*******************************)
(*******************************)

module Label = struct
  type t =
    | Assign_info of [ `Assign | `Assume ]  (** property-focused simplification *)
    | Int of int  (** use this for any experimental label features *)
  [@@deriving compare, equal, show]
end

module Brel = struct
  type t = VEq | VGeq | VGt [@@deriving compare, equal]

  let show ?(label = Label.Assign_info `Assume) = function
    | VEq -> if Label.equal label (Assign_info `Assume) then "=" else ":="
    | VGeq -> ">="
    | VGt -> ">"
  ;;

  let pp ?(label = Label.Assign_info `Assume) fmt t = Pp.string fmt (show ~label t)
end

module Op1 = struct
  type t = VNeg | VBNot [@@deriving compare, equal]

  let show = function VNeg -> "~-" | VBNot -> "~"
  let pp fmt t = Pp.string fmt (show t)
end

module Op2 = struct
  type t =
    | VAdd
    | VSub
    | VMul
    | VDiv
    | VMod
    | VPower
    | VShiftL
    | VShiftR
    | VBXor
    | VBAnd
    | VBOr
    | VBVConcat  (** bitvector concatenation *)
  [@@deriving compare, equal]

  let show = function
    | VAdd -> "+"
    | VSub -> "-"
    | VMul -> "*"
    | VDiv -> "/"
    | VMod -> "%"
    | VPower -> "**"
    | VShiftL -> "<<"
    | VShiftR -> ">>"
    | VBXor -> "^"
    | VBOr -> "|"
    | VBAnd -> "&"
    | VBVConcat -> "^^"
  ;;

  let pp fmt t = Pp.string fmt (show t)
end

type vformula =
  | VTrue
  | VFalse
  | VNot of vformula
  | VAnd of vformula * vformula
  | VOr of vformula * vformula
  | VBinRel of Brel.t * vexp * vexp
  | Imply of vformula * vformula
  | SigmaEqual of Var.t * vexp (* left: mapping, right: uint *)
  | NoOverflow of Var.t
  | UntrustSum of Var.t * Var.t (* sum of untrustworthy accounts *)
  | UntrustSum2 of Var.t * Var.t * Var.t (* invsum, struct, member *)
  | ForAll of Var.t list * vformula
  | Label of Label.t * vformula (* 0: assignment, 1: assume *)
[@@deriving compare, equal, show]

and vexp =
  | VInt of Z.t
  | Str of string * Typ.t
  | VVar of Var.t
  | Read of vexp * vexp (* A[i] *)
  | Write of vexp * vexp * vexp (* A[i] := v, return A *)
  | VBinOp of Op2.t * vexp * vexp * Typ.t
  | VUnOp of Op1.t * vexp * Typ.t
  | VCast of Typ.t * vexp
  | VCond of vformula
  | Ite of vexp * vexp * vexp  (** if-then-else *)
  | Uninterp of string * vexp list * Typ.t  (** fname, args, return type *)
[@@deriving compare, equal, show]

let pp_vformula_machine = pp_vformula
let pp_vexp_machine = pp_vexp
let compare_vf = compare_vformula
let compare_ve = compare_vexp
let equal_vf = equal_vformula
let equal_ve = equal_vexp

module FormulaSet = Set.Make (struct
  type t = vformula

  let compare = compare_vf
end)

module ExpSet = Set.Make (struct
  type t = vexp

  let compare = compare_ve
end)

module FormulaMap = Map.Make (struct
  type t = vformula

  let compare = compare_vf
end)

module ExpMap = Map.Make (struct
  type t = vexp

  let compare = compare_ve
end)

(** e.g., org ("x(#1)") = "x", org "y" = "y" *)
let org : Var.t -> Var.t =
 fun (x, t) -> try (fst (String.split x ~by:"(#"), t) with Not_found -> (x, t)
;;

(** e.g. [is_primed "x(#1)" = true], [is_primed "z" = false]*)
let is_primed (var : Var.t) =
  try
    let _ = String.split (fst var) ~by:"(#" in
    true
  with Not_found -> false
;;

let list_of_conjuncts ?(kill_label = true) (vf : vformula) : vformula list =
  let rec aux vf acc =
    match vf with
    | VTrue | VFalse | VNot _ | VOr _ | VBinRel _ | Imply _ | SigmaEqual _ | NoOverflow _
    | UntrustSum _ | UntrustSum2 _ | ForAll _ ->
      vf :: acc
    | VAnd (f1, f2) ->
      let acc = aux f1 acc in
      let acc = aux f2 acc in
      acc
    | Label (_, f) -> if kill_label then aux f acc else vf :: acc
  in
  aux vf [] |> List.rev
;;

(*******************************)
(* tostring and print          *)
(*******************************)

let rec to_string_vformula ?(version = 1) ?(show_label = true) vf =
  match vf with
  | VTrue -> "true"
  | VFalse -> "false"
  | VNot f -> "!(" ^ to_string_vformula f ^ ")"
  | VAnd (f1, f2) -> "(" ^ to_string_vformula f1 ^ " /\\ " ^ to_string_vformula f2 ^ ")"
  | VOr (f1, f2) -> "(" ^ to_string_vformula f1 ^ " \\/ " ^ to_string_vformula f2 ^ ")"
  | VBinRel (vbrel, ve1, ve2) -> (
    match vbrel with
    | VGeq -> "(" ^ to_string_vexp ~version ve1 ^ " >= " ^ to_string_vexp ~version ve2 ^ ")"
    | VGt -> "(" ^ to_string_vexp ~version ve1 ^ " > " ^ to_string_vexp ~version ve2 ^ ")"
    | VEq -> "(" ^ to_string_vexp ~version ve1 ^ " == " ^ to_string_vexp ~version ve2 ^ ")")
  | Imply (f1, f2) -> "(" ^ to_string_vformula f1 ^ " -> " ^ to_string_vformula f2 ^ ")"
  | SigmaEqual ((m, _t), e) -> "(Σ" ^ m ^ " == " ^ to_string_vexp ~version e ^ ")"
  | NoOverflow (x, _t) -> "NoOverflow (Σ" ^ x ^ ")"
  | UntrustSum ((isum, _), (m, _)) -> isum ^ " >= " ^ "Σ_u " ^ m
  | UntrustSum2 ((isum, _), (s, _), (mem, _)) ->
    isum ^ " >= " ^ "Σ_u,i " ^ mem ^ "[" ^ s ^ "[i]" ^ "]"
  | ForAll (vars, f) ->
    "(" ^ "forAll " ^ string_of_list Vocab.id (List.map fst vars) ^ "." ^ to_string_vformula f ^ ")"
  | Label (label, f) ->
    if show_label then
      if Label.equal label (Assign_info `Assign) then
        Format.sprintf "$assign{%s}" (to_string_vformula f)
      else to_string_vformula f
    else to_string_vformula f

and to_string_vexp ?(version = 1) ve =
  let to_string_vexp = to_string_vexp ~version in
  match ve with
  | VInt n -> Z.to_string n
  | Str (s, _) -> String.quote s
  | VVar v -> Var.show v
  | Read (ve1, ve2) ->
    if version = 1 then "Read" ^ "(" ^ to_string_vexp ve1 ^ ", " ^ to_string_vexp ve2 ^ ")"
    else Printf.sprintf "%s[%s]" (to_string_vexp ve1) (to_string_vexp ve2)
  | Write (ve1, ve2, ve3) ->
    if version = 1 then
      "Write" ^ "(" ^ to_string_vexp ve1 ^ ", " ^ to_string_vexp ve2 ^ ", " ^ to_string_vexp ve3
      ^ ")"
    else
      Printf.sprintf "%s[%s <| %s]" (to_string_vexp ve1) (to_string_vexp ve2) (to_string_vexp ve3)
  | VBinOp (vbop, ve1, ve2, _) -> (
    match vbop with
    | VAdd -> "(" ^ to_string_vexp ve1 ^ " + " ^ to_string_vexp ve2 ^ ")"
    | VSub -> "(" ^ to_string_vexp ve1 ^ " - " ^ to_string_vexp ve2 ^ ")"
    | VMul -> "(" ^ to_string_vexp ve1 ^ " * " ^ to_string_vexp ve2 ^ ")"
    | VDiv -> "(" ^ to_string_vexp ve1 ^ " / " ^ to_string_vexp ve2 ^ ")"
    | VMod -> "(" ^ to_string_vexp ve1 ^ " % " ^ to_string_vexp ve2 ^ ")"
    | VPower -> "(" ^ to_string_vexp ve1 ^ " ** " ^ to_string_vexp ve2 ^ ")"
    | VShiftL -> "(" ^ to_string_vexp ve1 ^ " << " ^ to_string_vexp ve2 ^ ")"
    | VShiftR -> "(" ^ to_string_vexp ve1 ^ " >> " ^ to_string_vexp ve2 ^ ")"
    | VBXor -> "(" ^ to_string_vexp ve1 ^ " ^ " ^ to_string_vexp ve2 ^ ")"
    | VBAnd -> "(" ^ to_string_vexp ve1 ^ " & " ^ to_string_vexp ve2 ^ ")"
    | VBOr -> "(" ^ to_string_vexp ve1 ^ " | " ^ to_string_vexp ve2 ^ ")"
    | VBVConcat -> "(" ^ to_string_vexp ve1 ^ " ~~ " ^ to_string_vexp ve2 ^ ")")
  | VUnOp (vuop, ve, _) -> (
    match vuop with
    | VNeg -> "(- " ^ to_string_vexp ve ^ ")"
    | VBNot -> "(~ " ^ to_string_vexp ve ^ ")")
  | VCast (typ, ve) -> Typ.to_string typ ^ "(" ^ to_string_vexp ve ^ ")"
  | VCond f -> to_string_vformula f
  | Ite (e1, e2, e3) ->
    "Ite(" ^ to_string_vexp e1 ^ ", " ^ to_string_vexp e2 ^ ", " ^ to_string_vexp e3 ^ ")"
  | Uninterp (fname, args, _) ->
    fname ^ string_of_list ~first:"(" ~sep:", " ~last:")" to_string_vexp args
;;

let rec pp_vformula ?(label = Label.Assign_info `Assume) fmt (vf : vformula) =
  let open Pp in
  let pp = pp_vformula ~label in
  match vf with
  | VTrue -> string fmt "true"
  | VFalse -> string fmt "false"
  | VNot vf -> pf fmt "!(%a)" pp vf
  | VAnd (l, r) -> pf fmt "(%a /\\ %a)" pp l pp r
  | VOr (l, r) -> pf fmt "(%a \\/ %a)" pp l pp r
  | VBinRel (brel, l, r) -> pf fmt "(%a %a %a)" pp_vexp l (Brel.pp ~label) brel pp_vexp r
  | Imply (l, r) -> pf fmt "(%a -> %a)" pp l pp r
  | ForAll (l, vf) -> pf fmt "forall %a.(%a)" (list Var.pp) l pp vf
  | Label ((Assign_info _ as l), vf) -> pp_vformula ~label:l fmt vf
  | Label (Int i, vf) -> pf fmt "#%i[%a]" i pp vf
  | _ -> raise NotImplemented

and pp_vexp fmt (ve : vexp) =
  let open Pp in
  let pp = pp_vexp in
  match ve with
  | VInt n -> z fmt n
  | Str (s, _) -> Pp.pf fmt "%s" (String.quote s)
  | VVar v -> Var.pp fmt v
  | Read (a, i) -> pf fmt "%a[%a]" pp a pp i
  | Write (a, i, v) -> pf fmt "%a[%a<|%a]" pp a pp i pp v
  | VBinOp (o, l, r, _) -> pf fmt "(%a %a %a)" pp l Op2.pp o pp r
  | VUnOp (o, e, _) -> pf fmt "%a(%a)" Op1.pp o pp e
  | VCast (t, e) -> pf fmt "%a(%a)" Typ.pp_short t pp e
  | VCond vf -> pp_vformula fmt vf
  | Ite (i, t, e) -> pf fmt "ITE(%a,%a,%a)" pp i pp t pp e
  | Uninterp (fname, args, _) -> pf fmt "%s(%a)" fname (seq ~sep:comma pp) args
;;

let pp_vf_list fmt l = Pp.(list pp_vformula) fmt l
let pp_vformula fmt vf = pp_vf_list fmt (list_of_conjuncts ~kill_label:false vf)

(*******************************)
(* tostring and print end      *)
(*******************************)

(** [make_forall l vf] returns [forall l.vf[l]]. Works well even if [l = []]. *)
let make_forall : Var.t list -> vformula -> vformula =
 fun bound vf -> if bound = [] then vf else ForAll (bound, vf)
;;

(** [make_exists l vf] returns [exists l.vf[l]]. Works well even if [l = []] *)
let make_exists : Var.t list -> vformula -> vformula =
 fun bound vf -> if bound = [] then vf else VNot (make_forall bound (VNot vf))
;;

let rec get_typ_vexp (vexp : vexp) : Typ.t =
  match vexp with
  | VInt _ -> ConstInt
  | Str (_, t) -> t
  | VVar (_, typ) -> typ
  | Read (ve, _) -> Typ.range (get_typ_vexp ve)
  | Write (ve, _, _) -> get_typ_vexp ve
  | VBinOp (_, _, _, typ) -> typ
  | VCast (typ, _) -> typ
  | VCond _ -> EType Bool
  | VUnOp (_, _, typ) -> typ
  | Ite (_, e1, e2) ->
    let t1, t2 = (get_typ_vexp e1, get_typ_vexp e2) in
    let _ = assert (t1 = t2) in
    t1
  | Uninterp (_, _, typ) -> typ
;;

let rec free_vf_aux acc vf =
  match vf with
  | VTrue | VFalse -> acc
  | VNot f -> free_vf_aux acc f
  | VAnd (l, r) | VOr (l, r) | Imply (l, r) ->
    let acc = free_vf_aux acc l in
    free_vf_aux acc r
  | VBinRel (_, l, r) ->
    let acc = free_ve_aux acc l in
    free_ve_aux acc r
  | SigmaEqual (x, e) ->
    let acc = Set.add x acc in
    free_ve_aux acc e
  | NoOverflow x -> Set.add x acc
  | UntrustSum (x, y) ->
    let acc = Set.add x acc in
    Set.add y acc
  | UntrustSum2 (x, y, z) ->
    let acc = Set.add x acc in
    let acc = Set.add y acc in
    Set.add z acc
  | ForAll (vars, f) ->
    let free_vars = Set.diff (free_vf f) (Set.of_list vars) in
    Set.union acc free_vars
  | Label (_, f) -> free_vf_aux acc f

and free_ve_aux acc ve =
  match ve with
  | VInt _ -> acc
  | Str _ -> acc
  | VVar v -> Set.add v acc
  | VUnOp (_, e, _) | VCast (_, e) -> free_ve_aux acc e
  | Read (l, r) | VBinOp (_, l, r, _) ->
    let acc = free_ve_aux acc l in
    free_ve_aux acc r
  | Write (e1, e2, e3) | Ite (e1, e2, e3) ->
    let acc = free_ve_aux acc e1 in
    let acc = free_ve_aux acc e2 in
    free_ve_aux acc e3
  | VCond f -> free_vf_aux acc f
  | Uninterp (_, args, _) -> List.fold_left free_ve_aux acc args

and free_vf vf = free_vf_aux Set.empty vf
and free_ve ve = free_ve_aux Set.empty ve

let split_implication : vformula -> vformula * vformula =
 fun vf ->
  match vf with Imply (pre, con) -> (pre, con) | _ -> failwith "VeriSmart.Vlang.split_implication"
;;

(** Transform A -> B into A && (not B). *)
let negate_implication (vf : vformula) =
  match vf with
  | Imply (pre, con) -> VAnd (pre, VNot con)
  | _ -> failwith "VeriSmart.Vlang.negate_implication"
;;

let split_vc : vformula -> vformula * vformula =
 fun vf ->
  match vf with
  | VAnd (pgm_const, VNot safety) -> (pgm_const, safety)
  | _ -> failwith "VeriSmart.Vlang.split_vc"
;;

let join_conjuncts l = match l with [] -> VTrue | _ -> List.reduce (fun l r -> VAnd (l, r)) l

let rec has_label : vformula -> bool =
 fun vf ->
  match vf with
  | VTrue | VFalse -> false
  | VNot f -> has_label f
  | VAnd (f1, f2) -> has_label f1 || has_label f2
  | VOr (f1, f2) -> has_label f1 || has_label f2
  | VBinRel _ -> false
  | Imply (f1, f2) -> has_label f1 || has_label f2
  | SigmaEqual _ -> false
  | NoOverflow _ -> false
  | UntrustSum _ -> false
  | UntrustSum2 _ -> false
  | ForAll (_, f) -> has_label f
  | Label _ -> true
;;

let rec rm_label (vf : vformula) : vformula =
  let w = rm_label in
  match vf with
  | VTrue | VFalse | VBinRel _ | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ -> vf
  | VNot vf -> VNot (w vf)
  | VAnd (vl, vr) -> VAnd (w vl, w vr)
  | VOr (vl, vr) -> VOr (w vl, w vr)
  | Imply (vl, vr) -> Imply (w vl, w vr)
  | ForAll (var_list, vf) -> ForAll (var_list, w vf)
  | Label (_, vf) -> w vf
;;

let rec rewrite_vf (vformula : vformula) (target : vexp) (replacement : vexp) : vformula =
  match vformula with
  | VTrue | VFalse -> vformula
  | VNot vf -> VNot (rewrite_vf vf target replacement)
  | VAnd (vf1, vf2) -> VAnd (rewrite_vf vf1 target replacement, rewrite_vf vf2 target replacement)
  | VOr (vf1, vf2) -> VOr (rewrite_vf vf1 target replacement, rewrite_vf vf2 target replacement)
  | VBinRel (vbrel, ve1, ve2) ->
    VBinRel (vbrel, rewrite_ve ve1 target replacement, rewrite_ve ve2 target replacement)
  | Imply (vf1, vf2) -> Imply (rewrite_vf vf1 target replacement, rewrite_vf vf2 target replacement)
  | SigmaEqual ((m, t), e) ->
    let m' = if String.equal m (to_string_vexp target) then to_string_vexp replacement else m in
    let e' = rewrite_ve e target replacement in
    SigmaEqual ((m', t), e')
  | NoOverflow (x, t) ->
    if String.equal x (to_string_vexp target) then NoOverflow (to_string_vexp replacement, t)
    else vformula
  | UntrustSum ((x, xt), (m, mt)) ->
    let x' = if String.equal x (to_string_vexp target) then to_string_vexp replacement else x in
    let m' = if String.equal m (to_string_vexp target) then to_string_vexp replacement else m in
    UntrustSum ((x', xt), (m', mt))
  | UntrustSum2 ((x, xt), (s, st), (mem, memt)) ->
    let x' = if String.equal x (to_string_vexp target) then to_string_vexp replacement else x in
    let s' = if String.equal s (to_string_vexp target) then to_string_vexp replacement else s in
    let mem' =
      if String.equal mem (to_string_vexp target) then to_string_vexp replacement else mem
    in
    UntrustSum2 ((x', xt), (s', st), (mem', memt))
  | ForAll (vars, vf) -> ForAll (vars, rewrite_vf vf target replacement)
  | Label (l, vf) -> Label (l, rewrite_vf vf target replacement)

and rewrite_ve : vexp -> vexp -> vexp -> vexp =
 fun vexp target replacement ->
  match vexp with
  | VInt _ -> vexp
  | Str _ -> vexp
  | VVar _ -> if equal_ve vexp target then replacement else vexp
  | Read (e1, e2) -> Read (rewrite_ve e1 target replacement, rewrite_ve e2 target replacement)
  | Write (e1, e2, e3) ->
    Write
      ( rewrite_ve e1 target replacement,
        rewrite_ve e2 target replacement,
        rewrite_ve e3 target replacement )
  | VBinOp (vbop, e1, e2, typ) ->
    VBinOp (vbop, rewrite_ve e1 target replacement, rewrite_ve e2 target replacement, typ)
  | VUnOp (vuop, e, typ) -> VUnOp (vuop, rewrite_ve e target replacement, typ)
  | VCast (typ, e) -> VCast (typ, rewrite_ve e target replacement)
  | VCond f -> VCond (rewrite_vf f target replacement)
  | Ite (e1, e2, e3) ->
    Ite
      ( rewrite_ve e1 target replacement,
        rewrite_ve e2 target replacement,
        rewrite_ve e3 target replacement )
  | Uninterp (fname, args, typ) ->
    let args' = List.map (fun arg -> rewrite_ve arg target replacement) args in
    Uninterp (fname, args', typ)
;;

(** [weaken_vf vf target] removes 'target' var in [vf] *)
let rec weaken_vf vf target =
  match vf with
  | VTrue | VFalse -> vf
  | VNot f ->
    let f' = weaken_vf f target in
    if equal_vf f' VTrue then VTrue else VNot f'
  | VAnd (f1, f2) -> VAnd (weaken_vf f1 target, weaken_vf f2 target)
  | VOr (f1, f2) -> VOr (weaken_vf f1 target, weaken_vf f2 target)
  | VBinRel (_vbrel, e1, e2) ->
    if Set.mem target (Set.union (Set.map fst (free_ve e1)) (Set.map fst (free_ve e2))) then VTrue
    else vf
  | Imply (f1, f2) -> Imply (weaken_vf f1 target, weaken_vf f2 target)
  | SigmaEqual ((x, _), e) ->
    if String.equal x target then VTrue
    else if Set.mem target (Set.map fst (free_ve e)) then VTrue
    else vf
  | NoOverflow (x, _) -> if String.equal x target then VTrue else vf
  | UntrustSum ((x1, _t1), (x2, _t2)) ->
    if String.equal target x1 || String.equal target x2 then VTrue else vf
  | UntrustSum2 ((x1, _t1), (x2, _t2), (x3, _t3)) ->
    if String.equal target x1 || String.equal target x2 || String.equal target x3 then VTrue else vf
  | ForAll (vars, f) -> ForAll (vars, weaken_vf f target)
  | Label (l, f) -> Label (l, weaken_vf f target)
;;

let weaken_vf2 (vf : vformula) (targets : string list) : vformula =
  List.fold_left weaken_vf vf targets
;;

let get_bigint_v (ve : vexp) : Z.t =
  match ve with
  | VInt n -> n
  | VUnOp (VNeg, VInt n, ConstInt) -> Z.neg n
  | VUnOp (VBNot, VInt n, ConstInt) -> n
  | _ -> invalid_arg "VeriSmart.Vlang.get_bigint_v"
;;

(*************************************)
(*** General Recursive Works       ***)
(*************************************)

(** Given 'work functions' [work_vf] and [work_ve], returns a recursive work version of [work_vf]
    and [work_ve]. A 'work function' should define a base case of your work. For example, if you
    want to write a function that replaces all [VVar v] to [VInt 4], then you should write
    [match vf with VVar v' when v = v' -> VInt 4 | _ -> vf]. Your work function should behave like
    an identity function when the given argument is not your target. *)
let mk_recursive_work work_vf work_ve =
  let rec f (vf : vformula) =
    let prev_work =
      match vf with
      | VTrue | VFalse -> vf
      | VNot vf' -> VNot (f vf')
      | VAnd (l, r) -> VAnd (f l, f r)
      | VOr (l, r) -> VOr (f l, f r)
      | VBinRel (vbrel, l, r) -> VBinRel (vbrel, g l, g r)
      | Imply (l, r) -> Imply (f l, f r)
      | SigmaEqual (v, e) -> SigmaEqual (v, g e)
      | NoOverflow _ | UntrustSum _ | UntrustSum2 _ -> vf
      | ForAll (l, vf) -> ForAll (l, f vf)
      | Label (i, vf) -> Label (i, f vf)
    in
    work_vf prev_work
  and g (ve : vexp) =
    let prev_work =
      match ve with
      | VInt _ | Str _ | VVar _ -> ve
      | Read (e1, e2) -> Read (g e1, g e2)
      | Write (e1, e2, e3) -> Write (g e1, g e2, g e3)
      | VBinOp (vbop, l, r, vtyp) -> VBinOp (vbop, g l, g r, vtyp)
      | VUnOp (vuop, e, vtyp) -> VUnOp (vuop, g e, vtyp)
      | VCast (t, e) -> VCast (t, g e)
      | VCond vf -> VCond (f vf)
      | Ite (e1, e2, e3) -> Ite (g e1, g e2, g e3)
      | Uninterp (fn_name, args, ret_typ) -> Uninterp (fn_name, List.map g args, ret_typ)
    in
    work_ve prev_work
  in
  (f, g)
;;

(** See [mk_recursive_work]. *)
let mk_recursive_work_vf work_vf = fst @@ mk_recursive_work work_vf id

(** See [mk_recursive_work]. *)
let mk_recursive_work_ve work_ve = snd @@ mk_recursive_work id work_ve

(** Given 'accumulating functions' [work_vf] and [work_ve], returns a recursive work version of
    [work_vf] and [work_ve]. For example, if you want to count all subformulas in a formula, then
    you should write [let work_vf acc _vf = acc + 1]. *)
let mk_accumulator (work_vf : 'acc -> vformula -> 'acc) (work_ve : 'acc -> vexp -> 'acc) =
  let rec f (acc : 'acc) (vf : vformula) =
    let prev_work =
      match vf with
      | VTrue | VFalse -> acc
      | VNot vf -> f acc vf
      | VAnd (l, r) | VOr (l, r) | Imply (l, r) ->
        let acc' = f acc l in
        let acc'' = f acc' r in
        acc''
      | VBinRel (_, l, r) ->
        let acc' = g acc l in
        let acc'' = g acc' r in
        acc''
      | ForAll (_, vf) -> f acc vf
      | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ | Label _ -> raise NotImplemented
    in
    work_vf prev_work vf
  and g (acc : 'acc) (ve : vexp) =
    let prev_work =
      match ve with
      | VInt _ | Str _ | VVar _ -> acc
      | Read (l, r) | VBinOp (_, l, r, _) ->
        let acc = g acc l in
        let acc = g acc r in
        acc
      | Write (a, b, c) | Ite (a, b, c) ->
        let acc = g acc a in
        let acc = g acc b in
        let acc = g acc c in
        acc
      | VUnOp (_, e, _) | VCast (_, e) -> g acc e
      | VCond vf -> f acc vf
      | Uninterp (_, l, _) -> List.fold_left g acc l
    in
    work_ve prev_work ve
  in
  (f, g)
;;

let mk_accumulator_vf work_vf = fst @@ mk_accumulator work_vf const
let mk_accumulator_ve work_ve = snd @@ mk_accumulator const work_ve

(** Similar to [Vlang.rewrite_vf], but more general. That is, it works even if [target] is not a
    variable. *)
let replace_vf vf target rep =
  let rep_ve_base ve = if equal_ve ve target then rep else ve in
  let f, _ = mk_recursive_work id rep_ve_base in
  f vf
;;

(*************************************)
(*** Formula Split                 ***)
(*************************************)

(** [pick_free_vf vf] returns [Some v] if [vf] has free variables and [v] is one of it. If there are
    two or more variables, what will be selected is not specified. Otherwise, return [None]. Similar
    to [BatSet.choose_opt (free_vf vf)], but faster. *)
let rec pick_free_vf (vf : vformula) : Var.t option =
  match vf with
  | VTrue | VFalse -> None
  | VNot vf | Label (_, vf) -> pick_free_vf vf
  | VAnd (l, r) | VOr (l, r) | Imply (l, r) -> begin
    match pick_free_vf l with Some v -> Some v | None -> pick_free_vf r
  end
  | VBinRel (_, e1, e2) -> begin
    match pick_free_ve e1 with Some v -> Some v | None -> pick_free_ve e2
  end
  | SigmaEqual (v, _) | UntrustSum (v, _) | UntrustSum2 (v, _, _) | NoOverflow v -> Some v
  | ForAll _ ->
    let vars = free_vf vf in
    Set.choose_opt vars

and pick_free_ve (ve : vexp) : Var.t option =
  match ve with
  | VInt _ -> None
  | Str _ -> None
  | VVar v -> Some v
  | VUnOp (_, e, _) | VCast (_, e) -> pick_free_ve e
  | Read (e1, e2) | VBinOp (_, e1, e2, _) -> begin
    match pick_free_ve e1 with Some v -> Some v | None -> pick_free_ve e2
  end
  | Write (e1, e2, e3) | Ite (e1, e2, e3) -> begin
    match pick_free_ve e1 with
    | Some v -> Some v
    | None -> begin match pick_free_ve e2 with Some v -> Some v | None -> pick_free_ve e3 end
  end
  | VCond vf -> pick_free_vf vf
  | Uninterp (_, args, _) ->
    List.fold_left
      (fun (acc : Var.t option) e -> match acc with Some _ -> acc | None -> pick_free_ve e)
      None args
;;

(** [formula_split vf] returns [vf1; vf2; ...] where [vf1], [vf2], ... does not share any free
    variables. *)
let formula_split (vf : vformula) : vformula list =
  let l = list_of_conjuncts vf in
  let frees = l |> List.map free_vf |> List.map Set.to_list in
  let uf = Union_find.create 32 in
  let rec joiner (frees : Var.t list) : unit =
    match frees with
    | v1 :: v2 :: tl ->
      Union_find.union uf v1 v2;
      (joiner [@tailcall]) (v2 :: tl)
    | _ -> ()
  in
  List.iter joiner frees;
  let tbl : (Var.t, vformula list) BatHashtbl.t = BatHashtbl.create 8 in
  let classify (vf : vformula) : unit =
    let random_var = pick_free_vf vf in
    let random_var = random_var |? ("@@@dummy@@@", EType (UInt 256)) in
    let par = Union_find.find_parent uf random_var in
    BatHashtbl.replace tbl par
      (if BatHashtbl.mem tbl par then vf :: BatHashtbl.find tbl par else [ vf ])
  in
  List.iter classify l;
  let tbl = BatHashtbl.map (fun _ v -> List.rev v |> join_conjuncts) tbl in
  let ret = tbl |> BatHashtbl.values |> List.of_enum in
  ret
;;

(*************************************)
(*** Formula Split end             ***)
(*************************************)

(*************************)
(*** Make fresh symbol ***)
(*************************)

open struct
  module Counter_sym = Counter.M ()
end

let newsym = "@SYM__"
let gen_newsym typ : Var.t = (newsym ^ string_of_int (Counter_sym.gen ()), typ)

(**********************************************)
(*** Ghost variables for analyzing Solidity ***)
(**********************************************)

(** (=[@TU]) trusted user or non-vulnerable user *)
let trust_map : Var.t = ("@TU", Mapping (Address, EType Bool))

(** (=[@Invest]) addresses that have sent money to contracts.

    {b Deprecated.} Use [invest_sum] instead. *)
let invest_map : Var.t = ("@Invest", Mapping (Address, EType (UInt 256)))

(** (=[@B]) balances of addresses. *)
let eth_map : Var.t = ("@B", Mapping (Address, EType (UInt 256)))

(** (=[@this]) *)
let this_addr : Var.t = ("@this", EType Address)

(** (=[@attack]). main attacker. *)
let attack_addr : Var.t = ("@attack", EType Address)

let length_map = "@L" (* length variable may have different types *)

(** (=[@Invest_sum]) Total amount of moneys that sent by untrusted users. *)
let invest_sum : Var.t = ("@Invest_sum", EType (UInt 256))

(** (=[@Invest_sum@MEMO]) Used when generating VCs. Considered as a local variable. *)
let invest_sum_memo : Var.t = ("@Invest_sum@MEMO", EType (UInt 256))

let pay_amount_memo : Var.t = ("@Pay_amount@MEMO", EType (UInt 256))
let global_ghost_var_names = [ "@TU"; "@Invest_sum"; "@B"; "@this"; "@L" ]
let msg_sender : Var.t = ("msg.sender", EType Address)
let msg_value : Var.t = ("msg.value", EType (UInt 256))
let callee : Var.t = ("@callee", EType Address)
let tx_origin : Var.t = ("tx.origin", EType Address)
let block_coinbase : Var.t = ("block.coinbase", EType Address)
let block_timestamp : Var.t = ("block.timestamp", EType (UInt 256))
let block_number : Var.t = ("block.number", EType (UInt 256))
let keccak_map : Var.t = ("@KECCAK256", Mapping (String, EType (UInt 256)))
