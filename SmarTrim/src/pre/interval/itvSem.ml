open Itv
open ItvDom
open! Frontend
open Frontend.Lang
open Vocab
open Vlang

let rec eval_aexp : exp -> Mem.t -> Val.t 
= fun exp mem ->
  match exp with
  | V (Int n) -> (Itv (V n, V n), GTaint.bot, BTaint.bot)
  | Str _ -> (Itv.top, GTaint.bot, BTaint.bot)
  | Lv lv ->
    if List.mem (to_string_lv lv) Lang.keyword_vars || Lang.is_balance_keyword lv then
      if List.mem (to_string_lv lv) Lang.blk_keyword_vars then
        (Itv.top, Val.gtaint_of (Mem.find (loc_of lv) mem), BTaint.top)
      else Val.update_itv Itv.top (Mem.find (loc_of lv) mem) 
    else Mem.find (loc_of lv) mem 
  | Cast (_t,e) -> (* overapproximation by not performing wrap-aroud *)
    eval_aexp e mem
  | TypeInfo (typ,x,_einfo) ->
    (match typ,x with
     | EType (Contract _cid), "name" -> (Itv.top, GTaint.bot, BTaint.bot)
     | EType (Contract _cid), "interfaceId" -> (Itv.top, GTaint.bot, BTaint.bot)
     | EType (UInt _n), "max" -> (Itv.top, GTaint.bot, BTaint.bot)
     | EType (UInt _n), "min" -> (Itv (V zero, V zero), GTaint.bot, BTaint.bot)
     | _ -> raise NotImplemented)
  | UnOp (uop,e,_t) ->
    let v = eval_aexp e mem in
    eval_uop uop v
  | BinOp (bop,e1,e2,_einfo) ->
    let v1,v2 = eval_aexp e1 mem, eval_aexp e2 mem in
    eval_bop bop v1 v2
  | _ -> failwith ("eval_aexp : " ^ to_string_exp exp)

and eval_uop : Uop.t -> Val.t -> Val.t
= fun uop v ->
  match uop with
  | Pos -> v 
  | Neg -> Val.update_itv Itv.top v
  | LNot -> Val.bot
  | BNot -> Val.update_itv Itv.top v

and eval_bop : Bop.t -> Val.t -> Val.t -> Val.t
= fun bop v1 v2 ->
  let gtaint = GTaint.join (Val.gtaint_of v1) (Val.gtaint_of v2) in
  let btaint = BTaint.join (Val.btaint_of v1) (Val.btaint_of v2) in
  let itv1,itv2 = Val.itv_of v1, Val.itv_of v2 in
  match bop with
  | Add -> (Itv.plus itv1 itv2, gtaint, btaint) 
  | Sub -> (Itv.minus itv1 itv2, gtaint, btaint) 
  | Mul -> (Itv.times itv1 itv2, gtaint, btaint)
  | Div -> (Itv.divide itv1 itv2, gtaint, btaint)
    (* TODO: define more precise semantics for mod operation *)
  | Mod -> (Itv.top, gtaint, btaint)
  | Exponent -> (Itv.power itv1 itv2, gtaint, btaint)
  | ShiftL | ShiftR | BAnd | BOr | BXor
  | GEq | Gt | LEq | Lt | Eq | NEq | LAnd | LOr -> (Itv.top, gtaint, btaint)

(* XXX: should be changed in accordance with
 * XXX: 1) 'convert_lv' in semantics.ml
 * XXX: 2) 'get_target' in semantics.ml
 * XXX: 3) 'get_defs_lv' in funcDefVar.ml
 * XXX: 4) 'loc_of' in itvSem.ml *)
and loc_of : lv -> ItvDom.Loc.t
= fun lv ->
  match lv with
  | Var (id,vinfo) -> (id, vinfo.vtyp)
  | MemberAccess (Cast (_, Lv lv),_,_,_) -> loc_of lv
  | MemberAccess (Lv lv,_,_,_) -> loc_of lv
  | IndexAccess (Cast (_, Lv lv),_,_) -> loc_of lv (* e.g., bytes(input)[i] where type(input) = string *)
  | IndexAccess (Lv lv,_,_) -> loc_of lv
  | _ -> failwith ("loc_of in itvSem.ml : " ^ to_string_lv lv)

let update : ItvDom.Loc.t -> Val.t -> Mem.t -> Mem.t
= fun loc v mem -> Mem.weak_update loc v mem

(* always weakly updates *)
let rec assign (lv,e) mem =
  let lv_typ = get_type_lv lv in
  match lv,e with
  (* TODO: This is an array initialization statment. may need to move to other places *)
  (* TODO: The current position (assignment) seems not proper. *)
  | _, Lv (Tuple (eops,_)) when Typ.is_array lv_typ ->
    let mem' =
      if Typ.is_static_array lv_typ then
        let size = Z.of_int (Option.get (Typ.get_array_size lv_typ)) in
        Mem.weak_update (length_map, Typ.EType (UInt 256)) ((Itv (V size,V size)), GTaint.bot, BTaint.bot) mem
      else (* dynamic array *)
        Mem.weak_update (length_map, Typ.EType (UInt 256)) (Itv.top, GTaint.bot, BTaint.bot) mem
    in
    BatList.fold_lefti (fun acc i eop ->
      match eop with
      | Some e ->
        let t = Typ.get_array_elem lv_typ in (* type of "lv[i]" *)
        assign (IndexAccess (Lv lv,Some (V (Int (Z.of_int i))),t), e) acc
      | None -> acc
    ) mem' eops
  | Tuple (eops1,_), Lv (Tuple (eops2,_)) -> (* (a,b) := (1,2) *)
    List.fold_left2 (fun acc eop1 eop2 ->
      match eop1,eop2 with
      | Some (Lv lv'),Some e' -> assign (lv',e') acc
      | None,Some _ -> acc
      | _ -> failwith "itvSem.ml : invalid tuple assignment"
    ) mem eops1 eops2
  | _ -> update (loc_of lv) (eval_aexp e mem) mem

let handle_one_callee (callee : Func.t) (lvop,_e,args) mem =
  let params = callee.params in
  let ret_params = callee.ret_params in
  (* params <- args *)
  let mem = try assign (params_to_lv params, args_to_exp args) mem with NoParameters -> mem in
  (* lvop <- ret_params *)
  let mem =
    match lvop with
    | None -> mem
    | Some lv -> try assign (lv, Lv (params_to_lv ret_params)) mem with NoParameters -> mem in
  mem

let handle_fcall (global : Global.t) (caller : Func.t) (lvop,e,args) mem =
  let cid = caller.info.scope_s in
  let callees = FuncMap.find_matching_funcs cid e (List.map get_type_exp args) global.cnames global.fmap in
  BatSet.fold (fun callee _acc ->
    handle_one_callee callee (lvop,e,args) mem 
  ) callees mem

let handle_init_funcs (_global: Global.t) _ctx_cname (lvop,f,args) _loc mem =
  match lvop with
  | None -> raise (Failure ("handle_init_funcs1: " ^ f))
  | Some lv ->
    (match f with
     | "array_init"
     | "dbytes_init"
     | "string_init"
     | "contract_init" ->
       let vars = List.fold_left (fun acc arg -> BatSet.union (var_exp arg) acc) (var_lv lv) args in
       BatSet.fold (fun loc acc ->
         update loc (Itv.top, GTaint.bot, BTaint.bot) acc
       ) vars mem
     | "struct_init" -> failwith "struct_init : itvSem.ml" (* must be handled in preprocessing step. *)
     | _ -> raise (Failure ("handle_init_funcs2: " ^ f)))

let handle_abi (lvop,f,_args) mem =
  match lvop with
  | None -> mem
  | Some lv -> 
    (match f with
     | "encode" | "decode" | "encodePacked" | "encodeWithSelector" | "encodeWithSignature" ->
        let v = (Itv.top, GTaint.bot, BTaint.bot) in
        update (loc_of lv) v mem
     | _ -> failwith "handle_abi")

let handle_undefs global ctx_cname (lvop,exp,args) loc mem =
  match exp with
  | Lv (Var (f,_)) when List.mem f Lang.init_funcs ->
    handle_init_funcs global ctx_cname (lvop,f,args) loc mem
  (* | Lv (MemberAccess (Lv (Var ("abi",_)),f,_,_)) ->
    handle_abi (lvop,f,args) mem *)
  | _ -> (* similar to 'handle_abi' *)
    (match lvop with
     | None -> mem
     | Some lv ->
       let v = (Itv.top, GTaint.bot, BTaint.bot) in
       (match lv with
        | Tuple (eops,_) ->
          List.fold_left (fun acc eop ->
            match eop with
            | Some (Lv lv') -> update (loc_of lv') v acc
            | None -> acc
            | _ -> assert false
          ) mem eops
        | _ -> update (loc_of lv) v mem))

let eval_stmt : Global.t -> string -> Func.t -> Node.t -> Mem.t -> Mem.t
= fun global main_name func node mem ->
  let stmt = Cfg.find_stmt node (Func.cfg func) in
  let ctx_cname = func.info.scope_s in
  match stmt with
  | Assign (lv,e,_) -> assign (lv,e) mem
  | Decl lv ->
    if Typ.is_uintkind (get_type_lv lv) || Typ.is_sintkind (get_type_lv lv) then
      update (loc_of lv) (Itv (V zero, V zero), GTaint.bot, BTaint.bot) mem
    else
      update (loc_of lv) (Itv.top, GTaint.bot, BTaint.bot) mem

  | Call (lvop,e,args,_,_,loc) when FuncMap.is_undef_call global.fmap stmt ->
    handle_undefs global ctx_cname (lvop,e,args) loc mem

  | Call (lvop,e,args,_,_,_) when FuncMap.is_internal_call global.fmap global.cnames stmt ->
    let callee = Global.get_callee_of_intcall global func stmt in
    handle_one_callee callee (lvop,e,args) mem
    (* handle_fcall global func (lvop,e,args) mem *)

  | Extcall _ -> mem

  | Call (lvop,_e,_args,_,_,_) -> (* object call: since it may be abstract function, over-approximate it *)
    (match lvop with
     | None -> mem
     | Some (Tuple (eops,_)) ->
       List.fold_left (fun acc eop ->
         match eop with
         | Some (Lv lv) -> update (loc_of lv) (Itv.top, GTaint.bot, BTaint.bot) acc
         | None -> acc
         | _ -> assert false
       ) mem eops
     | Some lv -> update (loc_of lv) (Itv.top, GTaint.bot, BTaint.bot) mem)

  | Return (None,_) -> mem
  | Return (Some e, _) -> (* ret_params <- e *)
    let ret_params = func.ret_params in
    let lv = try params_to_lv ret_params with _ -> assert false in
    if BatString.equal func.info.scope_s main_name then
      assign (lv, e) mem
    else
      (* TODO: dynamic method call case. when we analyze method function call body and encounters a global var, *)
      (* TODO: the value of it may be bot, because we do not pass the its constructor path, e.g., cve-2018-13132 (line 332, 341) *)
      (* TODO: taint values for global variables are bot, because it is not affected by global variables from root contract *)
      (* TODO: but for block information, I have to check. *)
      let rec assign' lv mem =
        match lv with
        | Var _ | IndexAccess _ | MemberAccess _ ->
          update (loc_of lv) (Itv.top, GTaint.bot, BTaint.bot) mem
        | Tuple (eops,_) ->
          List.fold_left (fun acc eop ->
            match eop with
            | Some (Lv lv') -> assign' lv' acc
            | Some _ -> failwith "itvSem.ml : invalid tuple assignment2"
            | None -> acc
          ) mem eops in
      assign' lv mem
  | Assembly (lst,_) ->
    let lst = List.map fst lst in
    Mem.map (fun (x,_t) v ->
      if List.mem x lst then
        (* XXX: currently, effects for taint values are ignored *)
        (Itv.top, Val.gtaint_of v, Val.btaint_of v)
      else v 
    ) mem
  | Skip | Revert | Assume _ | Assert _ | Label _  -> mem
  | If _ | Seq _ | While _ | Break | Continue
  | Placeholder | Unchecked _ -> failwith ("itSem:eval_stmt : " ^ to_string_stmt stmt)
