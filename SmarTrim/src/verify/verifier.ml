open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open Vlang
open! Checker
open Semantics
open Vocab
open Report
open Synthesizer
open InvMap
open ItvDom
open Itv
open Model
open Options
open VerifyUtils

let reg_check_phase = ref false

let is_cnstr_path : Global.t -> Paths.t -> bool
= fun global path -> Func.is_constructor (FuncMap.find path.fkey global.fmap)

let make_extern_paths : Global.t -> Paths.Set.t -> Path2.Set.t
= fun global paths ->
  let init = Path2.Set.of_list (List.map (fun p -> (None,p)) (Paths.Set.to_list paths)) in
  let paths = Paths.Set.filter (fun p -> not (is_cnstr_path global p)) paths in
  let paths = Paths.Set.to_list paths in
  let vpaths = if BatSet.is_empty global.extcalls then Path2.Set.empty else Path2.Set.of_list (List.map (fun p -> (Some Node.extern, p)) paths) in
  Path2.Set.union init vpaths

let make_call_paths' : (Node.t * Fkey.t) -> Paths.Set.t -> Path2.Set.t
= fun (node,fkey) paths ->
  let paths = Paths.Set.filter (fun p -> fkey = p.fkey) paths in
  let paths = Paths.Set.to_list paths in
  let cpaths = Path2.Set.of_list (List.map (fun p -> (Some node, p)) paths) in
  cpaths

let make_call_paths : Global.t -> Paths.Set.t -> Path2.Set.t
= fun global paths ->
  BatSet.fold (fun cn acc ->
    let paths' = make_call_paths' cn paths in
    Path2.Set.union paths' acc
  ) global.callnodes Path2.Set.empty

let augment_paths : Global.t -> Paths.Set.t -> Path2.Set.t
= fun global paths ->
  let extern_paths = make_extern_paths global paths in
  let call_paths = make_call_paths global paths in
  Path2.Set.union call_paths extern_paths

let pre_of_internal : Global.t -> InvMap.t -> Func.t -> vformula
= fun global invmap f ->
  assert Func.(is_internal f || is_private f);
  let trans_inv = InvMap.find (Plain Node.trans) invmap in
  if BatSet.is_empty global.extcalls || Func.is_view_pure f then trans_inv
  else if Global.contain_extern global f then InvMap.find (Plain Node.extern) invmap
  else trans_inv

let post_of_internal : Global.t -> InvMap.t -> Func.t -> vformula
= fun global invmap f ->
  assert Func.(is_internal f || is_private f);
  let trans_inv = InvMap.find (Plain Node.trans) invmap in
  if BatSet.is_empty global.extcalls || Func.is_view_pure f then trans_inv
  else if Global.contain_extern global f then InvMap.find (Plain Node.extern) invmap
  else trans_inv

let pre_of_internal2 : Global.t -> InvMap.t -> Func.t -> vformula
= fun _global invmap f ->
  assert Func.(is_internal f || is_private f);
  (* let trans_inv = InvMap.find (Plain Lang.trans_node) invmap in *)
  let extern_inv = InvMap.find (Plain Node.extern) invmap in
  if Func.is_view_pure f then extern_inv
  else if contain_lock extern_inv then VFalse
  else extern_inv

let post_of_internal2 : Global.t -> InvMap.t -> Func.t -> vformula
= fun _global invmap f ->
  assert Func.(is_internal f || is_private f);
  (* let trans_inv = InvMap.find (Plain Lang.trans_node) invmap in *)
  let extern_inv = InvMap.find (Plain Node.extern) invmap in
  if Func.is_view_pure f then extern_inv
  else if contain_lock extern_inv then VFalse
  else extern_inv

let pre_of_public_at_call : Global.t -> InvMap.t -> SpecMap.t -> Func.t -> Node.t -> vformula
= fun _global invmap specmap f callnode ->
  assert Func.(is_public f || is_external f);
  let trans_inv = InvMap.find (Plain Node.trans) invmap in
  if not (Func.is_view_pure f) then SpecMap.find_pre callnode specmap
  else trans_inv

let post_of_public_at_call : Global.t -> InvMap.t -> SpecMap.t -> Func.t -> Node.t -> vformula
= fun _global invmap specmap f callnode ->
  assert Func.(is_public f || is_external f);
  let trans_inv = InvMap.find (Plain Node.trans) invmap in
  if not (Func.is_view_pure f) then SpecMap.find_post callnode specmap
  else trans_inv

let post_of_public2 : Global.t -> InvMap.t -> Func.t -> vformula
= fun _global invmap f ->
  assert Func.(is_public f || is_external f);
  let extern_inv = InvMap.find (Plain Node.extern) invmap in
  if contain_lock extern_inv && contain_lock_s extern_inv f.body then VFalse
  else extern_inv

let intcall_semantics ctx global invmap specmap callee node =
  match ctx with
  | Some n when n = Node.extern ->
    if Func.is_public callee || Func.is_external callee then post_of_public2 global invmap callee
    else post_of_internal2 global invmap callee
  | None | Some _ ->
    if Func.is_public callee || Func.is_external callee then post_of_public_at_call global invmap specmap callee node
    else post_of_internal global invmap callee

let invalidate_ret lvop vf =
  match lvop with
  | None -> vf
  | Some lv ->
    let def = BatSet.to_list (FuncDefUse.get_def_set_lv lv) in
    weaken_vf2 vf def

let handle_internal_call ctx (global : Global.t) invmap specmap callee lvop vf node =
  (* 1. remove terms that include variables which may be modified in callee *)
  let k = Func.fkey callee in
  let vf = weaken_vf2 vf (BatSet.to_list (FuncDefUse.find_d k global.f_defuse)) in
  (* 2. conjoin with transaction/external call-site invariant *)
  let post = intcall_semantics ctx global invmap specmap callee node in
  let vf = VAnd (vf, post) in
  (* 3. if lv exists, replace target var by true *)
  let final = invalidate_ret lvop vf in
  final

let get_cond_at_cutpoint : Global.t -> InvMap.t -> SpecMap.t -> Func.t -> Path2.t -> Node.t -> vformula
= fun global invmap specmap func (nop, _) node ->
  let cfg = Func.cfg func in
  match nop with
  | None ->
    if Cfg.is_loophead node cfg then InvMap.find (Plain node) invmap
    else
      let cname = func.info.scope_s in
      let trans_inv = InvMap.find (Plain Node.trans) invmap in
      if BatString.equal cname !main_contract then
        if Node.is_entry node && Func.is_constructor func then VTrue else
        if Node.is_entry node && Func.(is_public func || is_external func) then trans_inv else
        if Node.is_exit node && Func.(is_public func || is_external func) then trans_inv else
        if Node.is_entry node && Func.(is_internal func || is_private func) then pre_of_internal global invmap func else
        if Node.is_exit node && Func.(is_internal func || is_private func) then post_of_internal global invmap func else
        if is_external_call_node node cfg then InvMap.find (Plain Node.extern) invmap else
        if is_internal_call_node global.fmap global.cnames node cfg then
          let callee = Global.get_callee_of_intcall global func (Cfg.find_stmt node cfg) in
          if Func.is_public callee || Func.is_external callee then
            pre_of_public_at_call global invmap specmap callee node
          else pre_of_internal global invmap callee
        else failwith "get_cond_at_cutpoint : not a cutpoint !"
      else VTrue (* functions not in a main contract *)
  | Some ctx when ctx = Node.extern -> (* external context *)
    let extern_inv = InvMap.find (Plain ctx) invmap in
    if Cfg.is_loophead node cfg then InvMap.find (Ctx (ctx,node)) invmap
    else
      let cname = func.info.scope_s in
      if BatString.equal cname !main_contract then
        if Node.is_entry node && Func.is_constructor func then assert false else
        if Node.is_entry node && Func.(is_public func || is_external func) then extern_inv else
        if Node.is_exit node && Func.(is_public func || is_external func) then post_of_public2 global invmap func else
        if Node.is_entry node && Func.(is_internal func || is_private func) then pre_of_internal2 global invmap func else
        if Node.is_exit node && Func.(is_internal func || is_private func) then post_of_internal2 global invmap func else
        if is_external_call_node node cfg then extern_inv else
        if is_internal_call_node global.fmap global.cnames node cfg then
          let callee = Global.get_callee_of_intcall global func (Cfg.find_stmt node cfg) in
          if Func.is_public callee || Func.is_external callee then extern_inv
          else pre_of_internal2 global invmap callee
        else failwith "get_cond_at_cutpoint : not a cutpoint !"
      else VTrue (* functions not in a main contract *)
  | Some ctx -> (* static call context *)
    if Cfg.is_loophead node cfg then SpecMap.find_post ctx specmap
    else
      let cname = func.info.scope_s in
      (* if BatString.equal cname !main_contract then *)
      if List.mem cname global.base_names then (* E.g., reentrancy/0x60C6b5DC066E33801F2D9F2830595490A3086B4e_1: 'super' *)
        if Node.is_entry node && Func.is_constructor func then assert false else
        if Node.is_entry node && Func.(is_public func || is_external func) then SpecMap.find_pre ctx specmap else
        if Node.is_exit node && Func.(is_public func || is_external func) then SpecMap.find_post ctx specmap else
        if Node.is_entry node && Func.(is_internal func || is_private func) then assert false else
        if Node.is_exit node && Func.(is_internal func || is_private func) then assert false else
        if is_external_call_node node cfg then InvMap.find (Plain Node.extern) invmap else
        if is_internal_call_node global.fmap global.cnames node cfg then
          let callee = Global.get_callee_of_intcall global func (Cfg.find_stmt node cfg) in
          if Func.is_public callee || Func.is_external callee then
            pre_of_public_at_call global invmap specmap callee node
          else pre_of_internal global invmap callee (* not 'pre_of_internal2' ! *)
        else failwith "get_cond_at_cutpoint : not a cutpoint !"
      else VTrue (* functions not in a main contract *)

let tu_each ve tf =
  let typ = get_typ_vexp ve in
  match typ with
  | _ when Typ.is_address_kind typ -> VBinRel (VEq, Read (VVar trust_map, ve), tf)
  | Array (t,_) when Typ.is_address_kind t ->
    let bv : Var.t = ("@i", EType (UInt 256)) in
    ForAll ([bv], VBinRel (VEq, Read (VVar trust_map, Read (ve, VVar bv)), tf))
  | _ when Typ.is_contract typ -> VBinRel (VEq, Read (VVar trust_map, ve), tf)
  | _ -> assert false

let all_trusted : vexp list -> vformula
= fun vexps ->
  List.fold_left (fun acc ve ->
    let f = tu_each ve (VCond VTrue) in
    if equal_vf f VTrue then f else VAnd (acc, f)
  ) VTrue vexps

let all_untrusted : vexp list -> vformula
= fun vexps ->
  List.fold_left (fun acc ve ->
    let f = tu_each ve (VCond VFalse) in
    if equal_vf f VTrue then f else VAnd (acc, f)
  ) VTrue vexps

let get_addr_related_params : Var.t list -> Var.t list
= fun vars ->
  List.filter (fun (_, (typ : Typ.t)) ->
    match typ with
    | _ when Typ.is_address_kind typ -> true
    | Array (t,_) when Typ.is_address_kind t -> true
    | _ when Typ.is_contract typ -> true
    | _ -> false
  ) vars

let trusted_user_constraint : Global.t -> Func.t -> Paths.t -> Node.t -> vformula -> vformula
= fun global func _path node vf ->
  let params = List.map (fun (v,vinfo) -> (v,vinfo.vtyp)) func.params in
  let addr_params = List.map (fun v -> VVar v) (get_addr_related_params params) in
  let hc_addrs1 = List.map (fun n -> VCast (EType Address, VInt n)) (Z.Set.to_list global.hardcoded_addrs) in
  let hc_addrs2 = 
    List.filter (fun v -> Typ.is_address (snd v) && Global.is_constant_address global v) global.global_vars 
  in
  let hc_addrs2 = List.map (fun v -> VVar v) hc_addrs2 in
  let hc_addrs = hc_addrs1 @ hc_addrs2 in
  let trusted_base = [VVar this_addr; VCast (EType Address, VInt Z.zero)] @ hc_addrs in
  if Func.is_constructor func && Node.is_entry node then
    let t = all_trusted ((VVar msg_sender)::(addr_params @ trusted_base)) in
    VAnd (vf, t)
  else if Node.is_entry node then
    let t' = all_trusted trusted_base in
    let t = all_trusted ((VVar msg_sender)::addr_params) in
    let u = all_untrusted ((VVar msg_sender)::addr_params) in
    VAnd (vf, VAnd (t', VOr (t,u)))
  else if Cfg.is_loophead node (Func.cfg func) then
    let t = all_trusted trusted_base in
    VAnd (vf, t)
  else assert false

(* TODO: integrate with those in execTran.ml *)
let init_invest_if_cnstr : Func.t -> vformula
= fun func ->
  if Func.is_constructor func then
    VAnd (ForAll ([("@i", EType Address)], VBinRel (VEq, Read (VVar invest_map, VVar ("@i", EType Address)), VInt Z.zero)),
      VBinRel (VEq, VVar invest_sum, VCast (EType (UInt 256), VInt Z.zero)))
  else VTrue

let inc_invest_if_payable : bool -> Func.t -> vformula -> vformula
= fun is_entry func vf ->
  if Func.is_payable func && is_entry then
    (* Invest[msg.sender] := Invest[msg.sender] + msg.value *)
    (* Invest_sum := if (untrusted) Invest_sum + msg.value else Invest_sum *)
    let target1, target2 = VVar invest_map, VVar invest_sum in
    let rep1, rep2 = rename target1, rename target2 in
    let ve1, ve2 =
      Write (target1, VVar msg_sender, VBinOp (VAdd, Read (target1, VVar msg_sender), VVar msg_value, EType (UInt 256))),
      VBinOp (VAdd, target2, VVar msg_value, EType (UInt 256)) in
    let vf' = rewrite_vf (rewrite_vf vf target1 rep1) target2 rep2 in
    let ve1', ve2' = rewrite_ve ve1 target1 rep1, rewrite_ve ve2 target2 rep2 in
    let ite = Ite (VCond (VBinRel (VEq, Read (VVar trust_map, VVar msg_sender), VCond VFalse)), ve2', rep2) in
    let test = VBinRel (VGeq, VBinOp (VAdd, rep2, VVar msg_value, EType (UInt 256)), rep2) in
    VAnd (test,
      VAnd (vf', VAnd (VBinRel (VEq, target1, ve1'), VBinRel (VEq, target2, ite))))
  else vf

let investor_constraint : bool -> Func.t -> vformula -> vformula
= fun is_entry func vf ->
  let vf = VAnd (vf, init_invest_if_cnstr func) in
  let vf = inc_invest_if_payable is_entry func vf in
  vf

let this_sender_eq_possible : vformula -> vformula
= fun vf ->
  if !PatternAnalysis.may_call_itself then vf
  else VAnd (vf, VNot (VBinRel (VEq, VVar this_addr, VVar msg_sender)))

let calldata_length : Func.t -> vformula
= fun func ->
  let typs = List.map (fun (_,info) -> info.vtyp) func.params in
  let b = List.for_all Typ.(fun t -> is_uintkind t || is_sintkind t || is_address t || is_bool t) typs in
  if b then
    let size = 4 + (List.length typs * 32) in (* 4 bytes for function signature + #arg * 32  *)
    VBinRel (VEq, VVar ("msg.data.length", EType (UInt 256)), VInt (Z.of_int size))
  else VTrue

let get_startf_endf : Global.t -> InvMap.t -> SpecMap.t -> Path2.t -> vformula * vformula
= fun global invmap specmap vpath ->
  let path = snd vpath in
  let bp = path.basic_path in
  let func = FuncMap.find path.fkey global.fmap in
  let cfg = Func.cfg func in
  let (start, _, last) = (BatList.hd bp, Paths.get_mid bp, BatList.last bp) in
  let _ = assert (Node.is_entry start || Cfg.is_loophead start cfg) in
  let _ = assert (Node.is_exit last || Cfg.is_loophead last cfg || is_internal_call_node global.fmap global.cnames last cfg || is_external_call_node last cfg) in
  (* formula at starting node *)
  let startf = get_cond_at_cutpoint global invmap specmap func vpath start in
  let startf = Label (Assign_info `Assume, startf) in
  let startf = trusted_user_constraint global func path start startf in
  let startf = investor_constraint (Node.is_entry start) func startf in
  let startf = this_sender_eq_possible startf in
  let startf = VAnd (startf, calldata_length func) in
  (* formula at end node *)
  let endf = get_cond_at_cutpoint global invmap specmap func vpath last in
  let endf = Label (Assign_info `Assume, endf) in
  (startf, endf)

let finalize_safety_cond : vformula -> Query.t list -> Query.t list
= fun state qs ->
  List.map (fun q ->
    Query.{q with vc2 = match q.vc2 with Imply _ -> q.vc2 | _ -> Imply (state, q.vc2)}
  ) qs

let convert_stmt ctx invmap specmap : Global.t -> Func.t -> Node.t -> vformula * Query.t list -> vformula * Query.t list
= fun global curf node (vf,qs) ->
  let stmt = Cfg.find_stmt node (Func.cfg curf) in
  match stmt with
  | Call (lvop, _, _, _, _, _) when is_internal_call global.fmap global.cnames stmt ->
    let _ = assert (no_eth_gas_modifiers stmt) in
    let qs' = finalize_safety_cond vf qs in (* delayed safety checking for internal calls *)
    let callee = Global.get_callee_of_intcall global curf stmt in
    let vf' = handle_internal_call ctx global invmap specmap callee lvop vf node in
    (vf', qs')
  | Extcall _ ->
    let defset = List.fold_left (fun acc (fk, FuncDefUse.V.{ d; u = _; a = _ }) ->
                   if Func.is_constructor (FuncMap.find fk global.fmap) then acc
                   else BatSet.union d acc
                 ) BatSet.empty (FuncDefUse.bindings global.f_defuse) in
    let fields = Global.get_all_fields global in
    let field_names = List.map fst fields in
    let g_defs = BatSet.filter (fun d -> List.mem d (List.map fst global.global_vars) (* global variables *)
                                         || d = "@Invest_sum"                   (* may be altered in payable *)
                                         || List.mem d field_names              (* structure fields *)
                 ) defset in
    let qs' = finalize_safety_cond vf qs in (* delayed safety checking for external calls *)
    let vf' = List.fold_left weaken_vf vf (BatSet.to_list g_defs) in
    let vf' = VAnd (vf', InvMap.find (InvMap.Plain Node.extern) invmap) in
    (vf', qs')

  | _ -> Semantics.convert_stmt global curf stmt (vf,qs)

let gen_vc : Global.t -> InvMap.t -> SpecMap.t -> Path2.Set.t -> vformula * vformula -> Path2.t -> vformula * Query.t list
= fun global invmap specmap _vpaths (startf,endf) (ctx,path) ->
  let mid = path.basic_path |> Paths.get_mid in
  let func = FuncMap.find path.fkey global.fmap in
  let cfg = Func.cfg func in
  let extern_ctx = match ctx with Some n -> n = Node.extern | _ -> false in
  let (sp, qs) =
    List.fold_left (fun (acc_vf, acc_qs) node ->
      let new_qs = CollectQuery2.collect ~reg_check:!reg_check_phase ~extern_ctx:extern_ctx global cfg acc_vf path node in
      convert_stmt ctx invmap specmap global func node (acc_vf, acc_qs @ new_qs)
    ) (startf, []) mid in
  let qs = finalize_safety_cond sp qs in
  let formula = Imply (sp, endf) in
  (formula, qs)

let get_all_vcs : Global.t -> InvMap.t -> SpecMap.t -> Path2.Set.t ->
                      (Path2.t * vformula) list * (Path2.t * Query.t) list
= fun global invmap specmap vpaths ->
  Path2.Set.fold (fun vpath (acc_vc,acc_qs) ->
    let (startf, endf) = get_startf_endf global invmap specmap vpath in
    let (vc',qs') = gen_vc global invmap  specmap vpaths (startf,endf) vpath in
    let qs' = List.map (fun q' -> (vpath,q')) qs' in
    (acc_vc @ [(vpath,vc')], qs'@acc_qs)
  ) vpaths ([],[])

let get_numeric_info : Loc.t BatSet.t -> ItvDom.Mem.t -> vformula
= fun locs mem ->
  Mem.fold (fun (id,t) v acc ->
    let itv = Val.itv_of v in
    (* interval values, propagated to
     * non-numerical typed variables, should not be reflected *)
    if Itv.is_top itv || not (Typ.is_uintkind t) then acc else
    if not (BatSet.mem (id,t) locs) then acc
    else
      (match itv with
       | Itv (V l, V u) when Z.equal l u ->
         let f = VBinRel (VEq, VVar (id,t), VInt u) in
         VAnd (acc,f)
       | Itv (V l, V u) ->
         let f1 = VBinRel (VGeq, VInt u, VVar (id,t)) in
         let f2 = VBinRel (VGeq, VVar (id,t), VInt l) in
         VAnd (acc, VAnd (f1,f2))
       | _ -> acc) 
  ) mem VTrue

(* get numeric lenth informaion *)
let get_numeric_info2 : ItvDom.Mem.t -> ExpSet.t -> vformula
= fun mem len_exps ->
  let v = Mem.find ("length", Typ.EType (UInt 256)) mem in
  let itv = Val.itv_of v in
  match itv with
  | Itv (V l, V u) when Z.equal l u ->
    ExpSet.fold (fun e acc ->
      let r = VBinRel (VEq, e, VInt u) in
      if equal_vf acc VTrue then r
      else VAnd (acc, r)
    ) len_exps VTrue
  | _ -> VTrue

let rec collect_len_exp : vformula -> ExpSet.t
= fun vf ->
  match vf with
  | Imply (f1,f2) -> ExpSet.union (collect_len_exp f1) (collect_len_exp f2)
  | VAnd (f1,f2) -> ExpSet.union (collect_len_exp f1) (collect_len_exp f2)
  | VNot f -> collect_len_exp f
  | VBinRel (_,e1,e2) -> ExpSet.union (collect_len_exp_e e1) (collect_len_exp_e e2)
  | _ -> ExpSet.empty

and collect_len_exp_e : vexp -> ExpSet.t
= fun ve ->
  match ve with
  | VBinOp (_,e1,e2,_) -> ExpSet.union (collect_len_exp_e e1) (collect_len_exp_e e2)
  | Read (VVar ("length", _), VVar _) -> ExpSet.singleton ve
  | _ -> ExpSet.empty

let add_numeric_info : ItvDom.Mem.t -> vformula -> vformula 
= fun mem vf ->
  match vf with
  | Imply (pre,con) ->
    let locs = BatSet.union (free_vf pre) (free_vf con) in
    let len_exps = collect_len_exp vf in
    let f1 = get_numeric_info locs mem in
    let f2 = get_numeric_info2 mem len_exps in
    Imply (VAnd (pre,VAnd (f1,f2)), con)
  | _ -> assert false

let rec g_write : Global.t -> vformula -> bool
= fun global vf ->
  match vf with
  | VAnd (f1,f2) -> g_write global f1 || g_write global f2
  | Label (Assign_info `Assign, VBinRel (VEq, VVar x, _)) -> (* e.g., cve-2018-11411 *)
    let fields = BatMap.bindings global.struct_map |> List.map snd in
    let field_names = List.fold_left (fun acc members -> acc @ (List.map Var2.id members)) [] fields in
    if List.mem (org x) global.global_vars || List.mem (fst (org x)) field_names || fst (org x) = "@Invest_sum" then true
    (* if List.mem (org x) global.gvars then true *)
    else false
  | _ -> false

let rec include_ret_param_false : vformula -> bool
= fun vf ->
  match vf with
  | VAnd (f1,f2) -> include_ret_param_false f1 || include_ret_param_false f2
  | Label (Assign_info `Assign, VBinRel (VEq, VVar x, VCond VFalse)) when BatString.starts_with (fst x) "success__@" || BatString.starts_with (fst x) Translator.param_name -> true
  | VBinRel (VEq, VVar x, VCond VFalse)
    (* likely ret param is false at the end *)
    when BatString.starts_with (fst x) "success__@" -> true
  | _ -> false

let rec include_ret_param_true : vformula -> bool
= fun vf ->
  match vf with
  | VAnd (f1,f2) -> include_ret_param_true f1 || include_ret_param_true f2
  | Label (Assign_info `Assign, VBinRel (VEq, VVar x, VCond VTrue)) when BatString.starts_with (fst x) "success__@"  || BatString.starts_with (fst x) Translator.param_name -> true
  | VBinRel (VEq, VVar x, VCond VTrue)
    (* likely ret param is false at the end *)
    when BatString.starts_with (fst x) "success__@" || BatString.starts_with (fst x) Translator.param_name -> true
  | _ -> false

let known_addition_pattern : vformula -> bool (* focus on common pattern *)
= fun vf ->
  match vf with
  | VBinRel (VGeq, VBinOp (VAdd, _e, VVar var, EType (UInt 256)), _e')
    when BatString.starts_with (fst var) "_value__@" || BatString.starts_with (fst var) "value__@"
    -> true
  | _ -> false

let ends_with_fexit : Paths.t -> bool
= fun path ->
  let bp = path.basic_path in
  let last = BatList.last bp in
  Node.is_exit last

let is_query_from_main_contract : Query.t -> bool
= fun q -> (fun Fkey.{ contract = c; func = _; _ } -> c = !main_contract) q.src_f

(* @return true : safe
 * @return false : don't know *)
let inspect_query_one : Global.t -> Mem.t -> Path2.t -> Query.t -> bool * Z3.Model.model option
= fun global mem _path q ->
  match q.kind with
  | IO ->
    let vc = if !refined_vcgen then q.vc2 else q.vc in
    let final = add_numeric_info mem vc in
    let (pre,con) = split_implication final in
    (* let addrs = addrs_vf con in *)
    let (b,mop) = is_valid_wrapper final in
    if b then (b,mop)
    else if !alarm_filter && !refined_vcgen && not (g_write global pre)
            && include_ret_param_false pre && not (include_ret_param_true pre)
            && known_addition_pattern con
            && ends_with_fexit q.path && is_query_from_main_contract q then
      let _ = print_endline ("\nWarning : conditional safety checking") in
      (true,None)
    else (b,mop)
  | DZ ->
    let final = add_numeric_info mem q.vc in
    is_valid_wrapper final
  | ASSERT ->
    let final = add_numeric_info mem q.vc in
    is_valid_wrapper final
  | SU -> is_valid_wrapper q.vc
  | EL ->
    (if not !refined_vcgen then q.vc
     else
       (match q.org_q with
        | Org_stmt (Call (_,Lv (Var (fname,_)),_,_,_,_))
          when List.mem fname ["selfdestruct";"suicide"] -> q.vc
        | Org_stmt (Call (_,Lv (MemberAccess (e,fname,_,_)),_,_,_,_))
          when Typ.is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] -> q.vc2
        | Org_stmt (Call (_, Lv (MemberAccess (e,"call",_,_)), _, Some _, _, _))
          when Typ.is_address_kind (get_type_exp e) -> q.vc2
        | _ -> assert false))
    |> is_valid_wrapper
  | RE_EL | RE ->
    let vc = if !refined_vcgen then q.vc2 else q.vc in
    is_valid_wrapper vc
  | TX_ORG ->
    let vc = if !refined_vcgen then q.vc2 else q.vc in
    is_valid_wrapper vc
  | NO_EFFECT (* necessary to reject regression in 2018-11411 *)
  | DEAD ->
    let final = add_numeric_info mem q.vc in
    is_valid_wrapper final
  | ASSIGN_CONST ->
    let final = add_numeric_info mem q.vc in
    is_valid_wrapper final
  | COV | ERC20 -> raise NotImplemented

exception Encountered_Unsafe_Query of Paths.t * string

let proven : Query.ID.t -> QMap.t -> bool
= fun qid qmap ->
  QMap.find qid qmap
  |> (fun (stat,_,_) -> stat = Proven)

let fkeys_of_paths : Paths.Set.t -> Fkey.t BatSet.t
= fun paths ->
  Paths.Set.fold (fun path acc ->
    BatSet.add path.fkey acc
  ) paths BatSet.empty

(* all queries in 'qs_bundle' have same Query.t ids. *)
let inspect_query_bundle : 
  Global.t -> Mem.t -> (Path2.t * Query.t) list -> Query.Status.t * Path2.Set.t
= fun global mem qs_bundle ->
  List.fold_left (fun (acc, acc_c, failed) (path,q) ->
    let cfg = FuncMap.find (Path2.get_fkey path) global.fmap |> Func.cfg in
    let comps = Component.collect_bp global (Path2.get_bp path) cfg in
    (* if failed && Component.subset comps acc_c then (acc, acc_c, failed) *)
    if failed (* && Component.subset comps acc_c *) then (Path2.Set.add path acc, acc_c, failed)
    else
      let (proven,_) = inspect_query_one global mem path q in
      match proven with
      | true -> (acc, acc_c, failed)
      | false -> (Path2.Set.add path acc, Component.union comps acc_c, true)
  ) (Path2.Set.empty, Component.empty_comps, false) qs_bundle
  |> (fun (a,_,failed) ->
      if failed then (Query.Status.Unproven, a)
      else (assert (Path2.Set.is_empty a); (Proven, a)))

let compare_query' (_,q1) (_,q2) = Query.compare q1 q2
let group' pairs = BatList.group compare_query' pairs

let verify_safety : Global.t -> Mem.t -> (Path2.t * Query.t) list -> QMap.t ->
                    Path2.Set.t * QMap.t * ModelMap.t
= fun global mem qs qmap_prev ->
  let qss = group' qs in
  BatList.fold_lefti (fun (acc_p, acc_qmap, acc_m, regression_found) i qs_bundle ->
    let qid = Query.get_qid (snd (List.hd qs_bundle)) in
    if !verbose >= 2 then begin
      prerr_string (string_of_int (i+1) ^ "/" ^ string_of_int (List.length qss) ^ " ... ");
      prerr_string ("[" ^ Kind.show qid.kind ^ "]" ^ " "); 
      prerr_string ("line " ^ string_of_int qid.loc.line ^ ", " ^ qid.exp ^ " ... "); 
      flush stderr
    end;
    if proven qid qmap_prev then begin
      if !verbose >= 2 then prerr_endline "proven";
      (acc_p, acc_qmap, acc_m, regression_found)
    end
    else begin
      let (status, unproven_paths) = inspect_query_bundle global mem qs_bundle in
      if Path2.Set.is_empty unproven_paths then
        let _ = assert (status = Proven) in
        let reg_found = qid.kind = NO_EFFECT || qid.kind = ASSIGN_CONST || qid.kind = DEAD in
        let fkeys = List.map fst qs_bundle |> List.map snd |> Paths.Set.of_list |> fkeys_of_paths in
        let acc_qmap' = QMap.add qid (status, fkeys, !iter) acc_qmap in
        let _ = if !verbose >= 2 then prerr_endline (Query.Status.show status) in
        (acc_p, acc_qmap', acc_m, reg_found)
      else
        let _ = assert (status = Unproven) in
        let fkeys = List.map snd (Path2.Set.to_list unproven_paths) |> Paths.Set.of_list |> fkeys_of_paths in
        let acc_qmap' = QMap.add qid (status, fkeys, !iter) acc_qmap in
        let _ = if !verbose >= 2 then prerr_endline "unproven" in
        let acc_p' = Path2.Set.union unproven_paths acc_p in
        (acc_p', acc_qmap', acc_m, regression_found)
    end
  ) (Path2.Set.empty, qmap_prev, ModelMap.empty, false) qss
  |> (fun (a,b,c,_) -> (a,b,c))

let debug_verify path vc res =
  print_endline (Path2.to_string path);
  print_endline (to_string_vformula vc);
  print_endline (string_of_bool (fst res) ^ "\n")

let verify_inductiveness : Global.t -> Mem.t -> (Path2.t * vformula) list ->
                           Path2.Set.t * ModelMap.t
= fun global mem pairs ->
  List.fold_left (fun (acc_p, acc_m, acc_c, failed) (path,vc) ->
    (* if failed >= 3 (* && Component.subset comps acc_c *) then (PathSet2.add path acc_p, acc_m, acc_c, failed)
    else *)
    let vc = add_numeric_info mem vc in
    let proven = is_valid_wrapper vc in
    let cfg = FuncMap.find (Path2.get_fkey path) global.fmap |> Func.cfg in
    let comps = Component.collect_bp global (Path2.get_bp path) cfg in
    let _ = if !debug = "invmap" && not (fst proven) then debug_verify path vc proven in
    match proven with
    | true,_ -> (acc_p, acc_m, acc_c, failed)
    | false,Some _ -> (Path2.Set.add path acc_p, acc_m, Component.union comps acc_c, failed + 1)
    | false,None -> (Path2.Set.add path acc_p, acc_m, Component.union comps acc_c, failed + 1)
  ) (Path2.Set.empty,ModelMap.empty,Component.empty_comps,0) pairs
  |> (fun (a,b,_,_) -> (a,b))

let print_invmap inductive invmap specmap =
  if inductive && !verbose >= 2 then
    (prerr_endline "";
     prerr_endline ("=============== Invariants Found ===============");
     prerr_endline ("Iter: " ^ (string_of_int !iter) ^ " " ^
                    "Total elapsed : " ^ string_of_float (Sys.time () -. !Profiler.start_cpu));
     prerr_endline (InvMap.to_string invmap ^ "\n" ^ SpecMap.to_string specmap))
  else ()
    (* (prerr_endline "";
     prerr_endline (InvMap.to_string invmap)) *)

let verify : Global.t -> InvMap.t -> SpecMap.t -> Mem.t -> Path2.Set.t ->
             (Path2.t * vformula) list -> (Path2.t * Query.t) list -> QMap.t ->
             bool * Path2.Set.t * QMap.t * ModelMap.t
= fun global invmap specmap mem _vpaths vcs qs qmap_prev ->
  let (unproven_paths1, model_map) = verify_inductiveness global mem vcs in
  if not (Path2.Set.is_empty unproven_paths1) then (false, unproven_paths1, qmap_prev, model_map)
  else
    let _ = assert (Path2.Set.is_empty unproven_paths1) in
    let _ = print_invmap true invmap specmap in
    let (unproven_paths2, qmap, model_map) = verify_safety global mem qs qmap_prev in
    (true, unproven_paths2, qmap, model_map)

let rec unlikely : (Var.t * vexp) -> vformula -> bool
= fun (x,e) vf ->
  match vf with
  | VAnd (f1,f2) -> unlikely (x,e) f1 || unlikely (x,e) f2
  | SigmaEqual (x',e') ->
    if x = x' && not (equal_ve e e') then true
    else false
  | ForAll _ -> false
  | VNot _ | Imply _ | VOr _ | Label _ -> assert false
  | _ -> false

let rec cost_vf' : vformula -> vformula -> int
= fun whole vf ->
  match vf with
  | VTrue -> 0 | VFalse -> 5
  | VAnd (f1,f2) -> cost_vf' whole f1 + cost_vf' whole f2
  | VBinRel (VEq,Read (VVar ("@TU",_),_),_) -> 1
  | VBinRel (VEq,VVar _,VCond VTrue) -> 3
  | VBinRel (VEq,VVar _,VCond VFalse) -> 30
  | VBinRel (VEq,e1,e2) ->
    let vars = BatSet.union (free_ve e1) (free_ve e2) in
    let b = BatSet.exists (fun (x,_) -> BatString.exists x Inline.inline_mark) vars in
    if b then 200 else 40
  | VBinRel _ -> 50
  | SigmaEqual (x,e) -> if unlikely (x,e) whole then 200 else 20
  | NoOverflow _ -> 18
  | UntrustSum _ -> 15
  | UntrustSum2 _ -> 15
  | ForAll _ -> 2
  | VNot _ | Imply _
  | VOr _ | Label _ -> assert false

let cost_vf vf = cost_vf' vf vf

let cost : InvMap.t * SpecMap.t -> int
= fun (invmap,_specmap) ->
  let cost1 =
    BatMap.foldi (fun n f acc ->
      let penalty = if equal_vf f VFalse && n = Plain Node.extern then 200 else 0 in
      acc + (cost_vf f) + penalty
    ) invmap 0 in
  (* let cost2 = BatMap.fold (fun (pre,post) acc -> acc + cost_vf pre + cost_vf post) specmap 0 in *)
  cost1

module Workset = struct
  type work = InvMap.t * SpecMap.t
 
  let to_string_work : work -> string
  = fun (invmap,specmap) -> InvMap.to_string invmap ^ "\n" ^ SpecMap.to_string specmap

  module OrderedType = struct
    type t = work
    let compare t1 t2 = Stdlib.compare (cost t1) (cost t2)
  end
  
  module Heap = BatHeap.Make (OrderedType)
  
  (* type of workset : heap * (string set) *)
  type t = Heap.t * string BatSet.t
  let empty = (Heap.empty, BatSet.empty)
  
  let explored : work -> t -> bool
  = fun work (_,sset) -> BatSet.mem (to_string_work work) sset

  let add : work -> t -> t
  = fun work (heap,sset) ->
    if explored work (heap,sset) then (heap,sset)
    else
      (Heap.add work heap, BatSet.add (to_string_work work) sset)
 
  let choose : t -> (work * t) option
  = fun (heap,sset) ->
    try
      let elem = Heap.find_min heap in
      Some (elem, (Heap.del_min heap, sset))
    with
      | _ -> None

  let merge (invmap1,specmap1) (invmap2,specmap2) =
    (InvMap.merge invmap1 invmap2, SpecMap.merge specmap1 specmap2)

  let propagate : work -> t -> t
  = fun work (heap,sset) ->
    let lst = Heap.to_list heap in
    let workset = List.map to_string_work lst in
    List.fold_left (fun (acc_w,acc_ws,acc_v) w' ->
      let w' = merge w' work in
      if List.mem (to_string_work w') workset || not (explored w' (acc_w,acc_v)) then
        let acc_w' = if List.mem (to_string_work w') acc_ws then acc_w else Heap.add w' acc_w in
        (acc_w', (to_string_work w')::acc_ws, BatSet.add (to_string_work w') acc_v)
      else (acc_w,acc_ws,acc_v)
    ) (Heap.empty,[],sset) lst
    |> (fun (a,_,c) -> (a,c))

  let workset_info : t -> string
  = fun (heap,sset) ->
    "To explore : " ^ (string_of_int (Heap.size heap)) ^
    " Explored : " ^ (string_of_int (BatSet.cardinal sset))
end

(******************)
(******************)
(*** Main Loop  ***)
(******************)
(******************)

let verify_start_time = ref 0.0

let gen_vc_verbose global (invmap, specmap) vpaths =
  let _ = if !verbose >= 2 then Profiler.start "Generating VCs ... " in
  let (vcs, qs) = get_all_vcs global invmap specmap vpaths in
  let _ = if !verbose >= 2 then Profiler.finish "Generating VCs ... " in
  (vcs, qs)

let verify_verbose global (invmap, specmap) mem vpaths vcs qs qmap_prev =
  let _ = if !verbose >= 2 then Profiler.start "Checking validity of VCs ... " in
  let (inductive, unproven_paths, qmap, model_map) = verify global invmap specmap mem vpaths vcs qs qmap_prev in
  let _ = if !verbose >= 2 then (Profiler.finish "Checking validity of VCs ... "; prerr_endline "") in
  (inductive, unproven_paths, qmap, model_map)

let rec work : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> Workset.t ->
               QMap.t * InvMap.t * SpecMap.t -> QMap.t * InvMap.t * SpecMap.t
= fun global mem vpaths workset (qmap_prev, invmap_known, specmap_known) ->
  if Sys.time () -. !verify_start_time > float_of_int !verify_timeout then (qmap_prev, invmap_known, specmap_known)
  else
  let _ = iter := !iter + 1 in
  let _ = if !iter mod 10 = 0 then (prerr_string ("Iter : " ^ string_of_int !iter ^ " ");
                                    prerr_endline ((Workset.workset_info workset) ^ " Total elapsed : " ^ string_of_float (Sys.time () -. !Profiler.start_cpu))) in
  match Workset.choose workset with
  | None -> (qmap_prev, invmap_known, specmap_known)
  | Some (((invmap_cand, specmap_cand) as cand), remaining_workset) ->
    let _ = if !debug = "invmap" then prerr_endline ("Cost : " ^ string_of_int (cost cand) ^ "\n" ^ Workset.to_string_work cand) in
    let (vcs, qs) = gen_vc_verbose global cand vpaths in
    let (inductive, unproven_paths, qmap, _model_map) = verify_verbose global cand mem vpaths vcs qs qmap_prev in
    let _ = if !debug = "invmap" then prerr_endline ("Inductive ? : " ^ string_of_bool inductive ^ "\n") in

    if Path2.Set.is_empty unproven_paths then (qmap, invmap_cand, specmap_cand)
    else
      let refinements = refine global unproven_paths cand in
      let refinements = List.map (fun (i,s) -> (InvMap.simplify i, SpecMap.simplify s)) refinements in
      let new_workset = List.fold_left (fun acc r -> Workset.add r acc) remaining_workset refinements in
      let (new_workset, new_invmap_known, new_specmap_known) =
        if inductive then
          let (invmap_known', specmap_known') = Workset.merge cand (invmap_known,specmap_known) in
          (Workset.propagate cand new_workset, invmap_known', specmap_known')
        else (new_workset, invmap_known, specmap_known) in
      work global mem vpaths new_workset (qmap, new_invmap_known, new_specmap_known)


let scan_node: Global.t -> Cfg.t -> Path2.t -> Node.t -> QMap.t -> QMap.t
= fun global g (ctx,path) node qmap ->
  let extern_ctx = match ctx with Some n -> n = Node.extern | _ -> false in
  let queries = CollectQuery2.collect ~reg_check:!reg_check_phase ~extern_ctx:extern_ctx global g VTrue path node in
  List.fold_left (fun acc q ->
    let k = QMap.mk_key q in
    let fkeys = try QMap.find k acc |> (fun (_,b,_) -> b) with Not_found -> BatSet.empty in
    QMap.add k (Query.Status.Unproven, BatSet.add path.fkey fkeys, 0) acc
  ) qmap queries

let scan_path: Global.t -> Path2.t -> QMap.t -> QMap.t
= fun global path qmap ->
  let (fk, mid) = (Path2.get_fkey path, path |> Path2.get_bp |> Paths.get_mid) in
  let cfg = Func.cfg (FuncMap.find fk global.fmap) in
  List.fold_left (fun acc n ->
    scan_node global cfg path n acc
  ) qmap mid
 
let init_qmap: Global.t -> Path2.Set.t -> QMap.t
= fun global paths ->
  Path2.Set.fold (fun p acc ->
    scan_path global p acc
  ) paths QMap.empty

let collect_cutpoints' : Global.t -> Paths.t ->
                         Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t ->
                         Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t
= fun global p (lhs,lhs2,lhs3,extcalls) ->
  let f = FuncMap.find p.fkey global.fmap in
  let cfg = Func.cfg f in
  let (hd,last) = (BatList.hd p.basic_path, BatList.last p.basic_path) in
  let b1 = Node.is_entry hd || Cfg.is_loophead hd cfg in
  let b2 = Node.is_exit last || Cfg.is_loophead last cfg || is_internal_call_node global.fmap global.cnames last cfg || is_external_call_node last cfg in
  let _ = assert (b1 && b2) in
  List.fold_left (fun (acc_lh,acc_lh2,acc_lh3,acc_ext) n ->
    if Cfg.is_loophead n cfg && not (Func.is_view_pure f) then
      if not (Global.contain_dead_intcall_cfg global f) then
        (BatSet.add n acc_lh, BatSet.add n acc_lh2, BatSet.add n acc_lh3, acc_ext)
      else (BatSet.add n acc_lh, BatSet.add n acc_lh2, acc_lh3, acc_ext)
    else if Cfg.is_loophead n cfg then (BatSet.add n acc_lh, acc_lh2, acc_lh3, acc_ext)
    else if is_external_call_node n cfg then (acc_lh, acc_lh2, acc_lh3, BatSet.add n acc_ext)
    else if is_internal_call_node global.fmap global.cnames n cfg then (acc_lh, acc_lh2, acc_lh3, acc_ext)
    else if Node.is_entry n || Node.is_exit n then (acc_lh, acc_lh2, acc_lh3, acc_ext)
    else assert false
  ) (lhs,lhs2,lhs3,extcalls) [hd;last]

let collect_cutpoints : Global.t -> Paths.Set.t ->
                        Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t * Node.t BatSet.t
= fun global paths ->
  Paths.Set.fold (collect_cutpoints' global)
    paths (BatSet.empty, BatSet.empty, BatSet.empty, BatSet.empty)

let collect_callnodes' : Global.t -> Paths.t -> (Node.t * Fkey.t) BatSet.t -> (Node.t * Fkey.t) BatSet.t
= fun global p callnodes ->
  let f = FuncMap.find p.fkey global.fmap in
  let cfg = Func.cfg f in
  List.fold_left (fun acc node ->
    if is_internal_call_node global.fmap global.cnames node cfg then
      let callee = Global.get_callee_of_intcall global f (Cfg.find_stmt node cfg) in
      if Func.(is_public callee || is_external callee) && not Func.(is_view_pure callee)
        then BatSet.add (node, Func.fkey callee) acc
      else acc
    else acc
  ) callnodes p.basic_path

let collect_callnodes : Global.t -> Paths.Set.t -> (Node.t * Fkey.t) BatSet.t
= fun global paths -> Paths.Set.fold (collect_callnodes' global) paths BatSet.empty

let init_invmap : Node.t BatSet.t * Node.t BatSet.t -> InvMap.t
= fun (lhs,extcalls) ->
  let l_extern = if BatSet.is_empty extcalls then BatSet.empty
                 else BatSet.map (fun l -> Ctx (Node.extern, l)) lhs in
  (* let l_call = BatSet.empty in *)
  let l_plain = BatSet.map (fun l -> Plain l) lhs in
  let cutpoints = if BatSet.is_empty extcalls then BatSet.add (Plain Node.trans) (BatSet.union l_extern l_plain)
                  else BatSet.add (Plain Node.extern) (BatSet.add (Plain Node.trans) (BatSet.union l_extern l_plain)) in
  BatSet.fold (fun cp acc -> InvMap.add cp VTrue acc) cutpoints InvMap.empty

let init_specmap : (Node.t * Fkey.t) BatSet.t -> SpecMap.t
= fun callnodes ->
  BatSet.fold (fun (n,_fk) acc -> SpecMap.add n (VTrue,VTrue) acc) callnodes SpecMap.empty


let augment_global : Paths.Set.t -> Global.t -> Global.t
= fun paths global ->
  let paths_main = Paths.Set.filter (fun { Paths.fkey = Fkey.{ contract = c; func = _; _ }; _} -> c = !main_contract) paths in
  let (lhs,lhs2,_,extcalls) = collect_cutpoints global paths in
  let (lhs_main, lhs2_main, lhs3_main, _) = collect_cutpoints global paths_main in
  let callnodes = collect_callnodes global paths in
  let callnodes_main = collect_callnodes global paths_main in
  {global with lhs=lhs; lhs2=lhs2; lhs_main=lhs_main; lhs2_main=lhs2_main; lhs3_main=lhs3_main;
               callnodes = callnodes; callnodes_main=callnodes_main; extcalls=extcalls}

let init : Global.t -> Path2.Set.t -> InvMap.t * SpecMap.t * Workset.t * QMap.t
= fun global vpaths ->
  let invmap0 = init_invmap (global.lhs, global.extcalls) in
  let specmap0 = init_specmap global.callnodes in
  let workset0 = Workset.add (invmap0,specmap0) Workset.empty in
  let qmap0 = init_qmap global vpaths in
  (invmap0, specmap0, workset0, qmap0)

(** adjust re alarms *)
let postprocess (qmap : QMap.t) : QMap.t =
  let all = QMap.bindings qmap in
  let unproven = get_unproven all in
  let unproven = adjust_re_alarms all unproven in
  List.fold_left (fun acc (k, ((stat,_,_) as v)) ->
    match stat with
    | Query.Status.Proven -> QMap.add k v acc
    | Unproven ->
      if List.mem (k,v) unproven then QMap.add k v acc
      else acc
    | Disproven | Unfalsifiable -> assert false
  ) QMap.empty all

(* usual verification routine *)
let do_verify : Global.t -> ItvDom.Mem.t -> Paths.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem paths ->
  verify_start_time := Sys.time();
  iter := 0;
  let global = augment_global paths global in
  let vpaths = augment_paths global paths in
  let (invmap0,specmap0,workset0,qmap0) = init global vpaths in
  let (qmap,invmap,specmap) = work global mem vpaths workset0 (qmap0, invmap0, specmap0) in
  let qmap = postprocess qmap in
  (qmap, QMap.empty, invmap, specmap)

(*******************************************)
(*** Sub-routines for Patch Verification ***)
(*******************************************)

let check_regression global mem (invmap,specmap) vpaths =
  let _ = reg_check_phase := true in
  let reg_qmap0 = init_qmap global vpaths in
  let (_,qs) = gen_vc_verbose global (invmap,specmap) vpaths in
  let _ = if !verbose >= 2 then Profiler.start "Checking validity of VCs ... " in
  let _ = print_endline "" in
  let (_,reg_qmap,_) = verify_safety global mem qs reg_qmap0 in
  let _ = if !verbose >= 2 then Profiler.finish "Checking validity of VCs ... " in
  let _ = print_endline "" in
  let _ = reg_check_phase := false in
  reg_qmap

let verify_org : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem vpaths ->
  let (invmap0,specmap0,workset0,qmap0) = init global vpaths in
  let (qmap,invmap,specmap) = work global mem vpaths workset0 (qmap0, invmap0, specmap0) in
  let reg_qmap = check_regression global mem (invmap,specmap) vpaths in
  (qmap, reg_qmap, invmap, specmap)

let verify_cand : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> QMap.t -> (InvMap.t * SpecMap.t) -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem vpaths qmap (invmap,specmap) ->
  let workset = Workset.add (invmap,specmap) Workset.empty in
  let (qmap,invmap,specmap) = work global mem vpaths workset (qmap, invmap, specmap) in
  let reg_qmap = check_regression global mem (invmap,specmap) vpaths in
  (qmap, reg_qmap, invmap, specmap)

let verify_inv_hold : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> (InvMap.t * SpecMap.t) -> bool
= fun global mem vpaths (invmap,specmap) ->
  let (vcs,_) = gen_vc_verbose global (invmap,specmap) vpaths in
  let (unproven_paths, _) = verify_inductiveness global mem vcs in
  let inductive = Path2.Set.is_empty unproven_paths in
  inductive

exception Solution_Inv_Not_Hold

(* XXX : necessary? already in verismart.json *)
let get_report_dir root = Filename.concat root "reports"
let get_vr root = Filename.concat (get_report_dir root) "vulnerability_report.csv"
let get_rr root = Filename.concat (get_report_dir root) "regression_report.csv"
let get_jr root = Filename.concat (get_report_dir root) "verismart.json"

let verify_when_solution_found : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> Paths.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem vpaths paths ->
  let (invmap0, specmap0, _, qmap0) = init global vpaths in
  let (success, invmap, specmap) =
    let invdir = get_jr !R.compdir in
    Parse.run global paths invdir (invmap0,specmap0) in

  if not success then (* parsing failed: usual patch verification *)
    let (qmap, reg_qmap, invmap, specmap) = verify_cand global mem vpaths qmap0 (invmap0,specmap0) in
    (qmap, reg_qmap, invmap, specmap)

  else (* differential verification *)
    let inductive = verify_inv_hold global mem vpaths (invmap,specmap) in
    if inductive then verify_cand global mem vpaths qmap0 (invmap,specmap)
    else raise Solution_Inv_Not_Hold

let verify_patch : Global.t -> ItvDom.Mem.t -> Path2.Set.t -> Paths.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem vpaths paths ->
  let solution_found =
    if !R.compdir = "" then false
    else Report.csv_report_alarms (get_vr !R.compdir) = 0 in
  if solution_found then
    verify_when_solution_found global mem vpaths paths
  else
    let (invmap0,specmap0,_,qmap0) = init global vpaths in
    verify_cand global mem vpaths qmap0 (invmap0,specmap0)

(* verification routine in repair mode *)
let do_repair_verify : Global.t -> ItvDom.Mem.t -> Paths.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem paths ->
  verify_start_time := Sys.time();
  iter := 0;
  let global = augment_global paths global in
  let vpaths = augment_paths global paths in
  if !R.overify then
    verify_org global mem vpaths
  else if !R.pverify then
    verify_patch global mem vpaths paths
  else assert false


let run : Global.t -> ItvDom.Mem.t -> Paths.Set.t -> (QMap.t * QMap.t * InvMap.t * SpecMap.t)
= fun global mem paths ->
  if !R.overify || !R.pverify then
    let _ = assert !Chk.reg in
    do_repair_verify global mem paths
  else
    do_verify global mem paths
