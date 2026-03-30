open! Frontend
open Itv
open ItvDom
open Vlang

let rec eval_ve (ve : vexp) (mem : Mem.t) : Val.t =
  match ve with
  | VInt n -> Val.of_itv (Itv (V n, V n))
  | Str _ -> raise Vocab.NotImplemented (* TODO, currently i dont know *)
  | VVar (x,t) -> Mem.find2 (x,t) mem 
  | Read _ -> Val.of_itv Itv.top
  | Write _ -> Val.of_itv Itv.top
  | VBinOp (bop,e1,e2,_) -> eval_bop bop (eval_ve e1 mem) (eval_ve e2 mem)
  | VUnOp (uop,e,_) -> eval_uop uop (eval_ve e mem)
    (* TODO : consider type conversions *)
  | VCast (_,e) -> eval_ve e mem
  | VCond _ -> Val.of_itv Itv.top
  | Ite _ -> Val.of_itv Itv.top
  | Uninterp _ -> Val.of_itv Itv.top

and eval_uop : Op1.t -> Val.t -> Val.t 
= fun _uop _v -> Val.of_itv Itv.top

and eval_bop : Op2.t -> Val.t -> Val.t -> Val.t 
= fun bop v1 v2 ->
  let i1 = Val.itv_of v1 in
  let i2 = Val.itv_of v2 in
    match bop with
    | VAdd -> Val.of_itv (Itv.plus i1 i2) 
    | VSub -> Val.of_itv (Itv.minus i1 i2)
    | VMul -> Val.of_itv (Itv.times i1 i2)
    | VDiv -> Val.of_itv (Itv.divide i1 i2)
    | VMod -> Val.of_itv (Itv.modulo i1 i2)
    | VPower -> Val.of_itv (Itv.power i1 i2)
    | VShiftL | VShiftR | VBXor | VBAnd | VBOr | VBVConcat -> Val.of_itv Itv.top

let rec eval_vf : vformula -> Mem.t -> Mem.t
= fun vf mem ->
  match vf with
  | VTrue -> mem
  | VFalse -> Mem.bot
  | VNot f -> eval_vf (neg_of f) mem
  | VAnd (f1,f2) -> Mem.meet (eval_vf f1 mem) (eval_vf f2 mem)
  | VOr (f1,f2) -> Mem.join (eval_vf f1 mem) (eval_vf f2 mem)
  | VBinRel (_, l, r) 
    when Typ.equal (get_typ_vexp l) (EType String)
         || Typ.equal (get_typ_vexp r) (EType String) ->
    (* do not perform itv on strings *)
    mem
  | VBinRel (brel,VVar (x,t),e) ->
    (match brel with
     | VGeq | VGt ->
       (try
          let i = Itv (V (Itv.lower_int (Val.itv_of (eval_ve e mem))), PInf) in
          Mem.update (x,t) (Val.of_itv i) mem
        with _ -> mem)
     | VEq -> Mem.update (x,t) (eval_ve e mem) mem)
  | VBinRel (brel,e,VVar (x,t)) ->
    (match brel with
     | VGeq | VGt ->
       (try
          let i = Itv (NInf, V (Itv.upper_int (Val.itv_of (eval_ve e mem)))) in
          Mem.update (x,t) (Val.of_itv i) mem
        with _ -> mem)
     | VEq -> Mem.update (x,t) (eval_ve e mem) mem)
  | VBinRel _ -> mem
  | Imply (f1,f2) -> eval_vf (VOr (VNot f1, f2)) mem
  | SigmaEqual _ -> mem
  | NoOverflow _ -> mem
  | UntrustSum _ -> mem
  | UntrustSum2 _ -> mem
  | ForAll _ -> mem (* may be encountered on testing mode, e.g., removing power op *)
  | Label (_,f) -> eval_vf f mem

and neg_of : vformula -> vformula
= fun vf ->
  match vf with
  | VTrue -> VFalse 
  | VFalse -> VTrue
  | VNot f -> f
  | VAnd (f1,f2) -> VOr (VNot f1, VNot f2)
  | VOr (f1,f2) -> VAnd (VNot f1, VNot f2)
  | VBinRel (brel,e1,e2) ->
    (match brel with
     | VGeq -> VBinRel (VGt,e2,e1)
     | VGt -> VBinRel (VGeq,e2,e1)
     | VEq -> VOr (VBinRel (VGt,e1,e2), VBinRel (VGt,e2,e1)))
  | Imply (f1,f2) -> VAnd (f1, VNot f2)
  | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ -> failwith "neg_of : itvSem2.ml"
  | ForAll _ -> raise (Failure "neg_of : itvSem2.ml") (* this constructor is only used for exploit generation *)
  | Label (_,f) -> neg_of f
