open! Frontend
open Frontend.Lang
open Vlang
open Frontend.FuncMap
open Component
open InvMap
open VerifyUtils
open! Vocab

let gen_conjunct ~kind : comps -> vformula list
= fun comps ->
  let mvars = BatSet.to_list comps.mapvars in
  let pairs = BatSet.to_list comps.mapmems in
  let reads = ExpSet.to_list comps.reads in
  let ivars = BatSet.to_list comps.ivars in
  let avars = BatSet.to_list comps.avars in
  let a_arrs = BatSet.to_list comps.a_arrs in
  let a_maps = BatSet.to_list comps.a_maps in
  let bvars = BatSet.to_list comps.bvars in
  let ints = Z.Set.to_list comps.ints in
  let ivars_mvars = BatList.cartesian_product ivars mvars in
  let ints_mvars = BatList.cartesian_product ints mvars in
  let ivars_ivars = BatList.cartesian_product ivars ivars in
  let ints_ivars = BatList.cartesian_product ints ivars in
  let reads_ivars = BatList.cartesian_product reads ivars in
  let l1 =
    List.fold_left (fun acc ((x,xt),(m,mt)) ->
      if Typ.is_uint256 xt then (SigmaEqual ((m,mt), VVar (x,xt)))::acc
      else acc 
    ) [] ivars_mvars in
  let l2 =
    List.fold_left (fun acc (n,(m,t)) ->
      (SigmaEqual ((m,t), VInt n))::acc
    ) [] ints_mvars in
  let l3 =
    List.fold_left (fun acc (m,t) ->
      (NoOverflow (m,t))::acc
    ) [] mvars in
  let l4 = (* x[y] = z *)
    List.fold_left (fun acc (r, (x,xt)) ->
      if xt = get_typ_vexp r then (VBinRel (VEq, r, VVar (x,xt)))::acc
      else acc
    ) [] reads_ivars in
  let l5 = (* x = y *)
    List.fold_left (fun acc ((x,xt),(y,yt)) ->
      if xt=yt then (VBinRel (VEq, VVar (x,xt), VVar (y,yt)))::acc
      else acc
    ) [] ivars_ivars in
  let l6 = (* x >= y *)
    List.fold_left (fun acc ((x,xt),(y,yt)) ->
      if xt=yt then (VBinRel (VGeq, VVar (x,xt), VVar (y,yt)))::acc
      else acc
    ) [] ivars_ivars in
  let l7 = (* x >= n *)
    List.fold_left (fun acc (n, (y,t)) ->
      (VBinRel (VGeq, VVar (y,t), VInt n))::acc
    ) [] ints_ivars in
  let l8 = (* n = x *)
    List.fold_left (fun acc (n, (y,t)) ->
      (VBinRel (VEq, VVar (y,t), VInt n))::acc
    ) [] ints_ivars in
  let l9 = (* n >= x *)
    List.fold_left (fun acc (n, (y,t)) ->
      (VBinRel (VGeq, VInt n, VVar (y,t)))::acc
    ) [] ints_ivars in
  let l10 = (* @TU[addr] = True *)
    List.fold_left (fun acc (x,t) ->
      (VBinRel (VEq, Read (VVar trust_map, VVar (x,t)), VCond VTrue))::acc
    ) [] avars in
  let l11 =
    List.fold_left (fun acc arr ->
      let bv : Var.t = ("@i", EType (UInt 256)) in
      (ForAll ([bv], VBinRel (VEq, Read (VVar trust_map, Read (VVar arr, VVar bv)), VCond VTrue)))::acc
    ) [] a_arrs in
  let l12 =
    List.fold_left (fun acc map ->
      let bv : Var.t = ("@i", EType Address) in
      let owner_trust = VBinRel (VEq, Read (VVar trust_map, VVar bv), VCond VTrue) in
      let parent_trust = VBinRel (VEq, Read (VVar trust_map, Read (VVar map, VVar bv)), VCond VTrue) in
      (ForAll ([bv], Imply (VNot (VBinRel (VEq, Read (VVar map, VVar bv), VInt Z.zero)),
                            VAnd (owner_trust, parent_trust))))::acc
    ) [] a_maps in
  let l13 =
    List.fold_left (fun acc map ->
      (UntrustSum (invest_sum, map))::acc
    ) [] mvars in
  let l14 =
    List.fold_left (fun acc (s,mem) ->
      (UntrustSum2 (invest_sum, s, mem))::acc
    ) [] pairs in
  let l15 =
    List.fold_left (fun acc (x,t) ->
      (VBinRel (VEq, VVar (x,t), VCond VTrue))::acc
    ) [] bvars in
  let l16 =
    List.fold_left (fun acc (x,t) ->
      (VBinRel (VEq, VVar (x,t), VCond VFalse))::acc
    ) [] bvars in
  let l17 = if List.length bvars > 0 then [VFalse] else [] in
  match kind with
  | "tran" -> l1@l2@l3@l4@l5@l6@l7@l8@l9@l10@l11@l12@l13@l14
    (* [] *)
  | "loop" -> l1@l2@l3@l4@l5@l6@l7@l8@l9@l10@l11@l12@l13@l14@l15@l16
    (* [] *)
  | "ext" -> l1@l2@l3@l4@l5@l6@l7@l8@l9@l10@l11@l12@l13@l14@l15@l16
  | "ext_loop" -> l15@l16@l17
    (* l17 *)
  | "ext_view_pure_loop" -> l15@l16
  | "pre" -> l15@l16
  | _ -> assert false

let next ~kind : comps -> InvMap.key -> InvMap.t -> InvMap.t list
= fun comps key invmap ->
  let old_f = InvMap.find key invmap in
  List.fold_left (fun acc f ->
    let new_f = Simplification.simplify (VAnd (old_f,f)) in
    (InvMap.add key new_f invmap)::acc
  ) [] (gen_conjunct ~kind:kind comps)

let next_tran = next ~kind:"tran"
let next_loop = next ~kind:"loop"
let next_ext = next ~kind:"ext"
let next_ext_loop = next ~kind:"ext_loop" (* TODO: enuemrate false only when ... extern inv contains lock *)
let next_ext_loop2 = next ~kind:"ext_view_pure_loop"

let next_spec : comps -> SpecMap.key -> SpecMap.t -> SpecMap.t list
= fun comps key specmap ->
  let (old_pre,_old_post) = SpecMap.find key specmap in
  List.fold_left (fun acc f ->
    let new_pre = Simplification.simplify (VAnd (old_pre,f)) in
    let new_post = VFalse in
    (SpecMap.add key (new_pre,new_post) specmap)::acc
  ) [] (gen_conjunct ~kind:"pre" comps)

let collect_comps_g : Global.t -> Path2.t -> comps
= fun global path ->
  let fk = Path2.get_fkey path in
  let bp = Path2.get_bp path in
  let cfg = Func.cfg (FuncMap.find fk global.fmap) in
  let comps_bp = collect_bp global bp cfg in
  let lst = List.filter (fun (Fkey.{ contract = c; func = f; _ }, _) ->
              BatString.equal c !Options.main_contract
              && BatString.equal c f
            ) (BatMap.bindings global.fmap) in
  let _ = assert (List.length lst = 1) in
  let cnstr = snd (List.hd lst) in
  let comps_cnstr = collect_f global cnstr in
  {mapvars = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.mapvars;
   mapmems = BatSet.union comps_cnstr.mapmems comps_bp.mapmems;
   reads = ExpSet.filter (fun ve ->
             let set1 = BatSet.map fst (free_ve ve) in
             let set2 = BatSet.map fst (BatSet.of_list global.global_vars) in
             BatSet.subset set1 set2
           ) comps_cnstr.reads;
   ivars = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.ivars;
   avars = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.avars;
   a_arrs = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.a_arrs;
   a_maps = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.a_maps;
   bvars = BatSet.filter (fun v -> List.mem v global.global_vars) comps_cnstr.bvars;
   ints = Z.Set.union comps_cnstr.ints comps_bp.ints}

let refine_tran : Global.t -> Path2.Set.t -> InvMap.t -> InvMap.t list
= fun global paths invmap ->
  Path2.Set.fold (fun (ctx, p) acc ->
    let (hd, last) = (BatList.hd p.basic_path, BatList.last p.basic_path) in
    let func = FuncMap.find p.fkey global.fmap in
    let comps_g = collect_comps_g global (ctx, p) in
    match ctx with
    | None ->
      if Func.(is_public func || is_external func) && (Node.is_entry hd || Node.is_exit last) then
        let cands = next_tran comps_g (Plain Node.trans) invmap in
        cands @ acc
      else acc
    | Some _ctx -> acc
  ) paths []

let refine_loop : Global.t -> Path2.Set.t -> InvMap.t -> InvMap.t list
= fun global paths invmap ->
  Path2.Set.fold (fun (ctx, p) acc ->
    let (hd, last) = (BatList.hd p.basic_path, BatList.last p.basic_path) in
    let func = FuncMap.find p.fkey global.fmap in
    let cfg = Func.cfg func in
    let comps_l = collect_bp global p.basic_path cfg in
    let comps_g = collect_comps_g global (ctx, p) in
    let extern_node_exists = List.exists (fun n -> is_external_call_node n cfg) p.basic_path in
    match ctx with
    | None ->
      let cand1 = if Cfg.is_loophead hd cfg then next_loop comps_l (Plain hd) invmap
                  else [] in
      let cand2 = if Cfg.is_loophead last cfg && extern_node_exists then next_loop comps_g (Plain last) invmap
                  else if Cfg.is_loophead last cfg then next_loop comps_g (Plain last) invmap
                  else [] in
      cand1 @ cand2 @ acc
    | Some ctx when ctx = Lang.Node.extern ->
      let cand1 = if Cfg.is_loophead hd cfg && Func.is_view_pure func then next_ext_loop2 comps_g (Ctx (ctx,hd)) invmap
                  else if Cfg.is_loophead hd cfg then next_ext_loop comps_g (Ctx (ctx,hd)) invmap
                  else [] in
      let cand2 = if Cfg.is_loophead hd cfg && Func.is_view_pure func then next_ext_loop2 comps_g (Ctx (ctx,hd)) invmap
                  else if Cfg.is_loophead last cfg && extern_node_exists then next_ext_loop comps_l (Ctx (ctx,last)) invmap
                  else if Cfg.is_loophead last cfg then next_ext_loop comps_g (Ctx (ctx,last)) invmap
                  else [] in
      cand1 @ cand2 @ acc
    | _ -> acc
  ) paths []

let refine_ext : Global.t -> Path2.Set.t -> InvMap.t -> InvMap.t list
= fun global paths invmap ->
  Path2.Set.fold (fun (ctx, p) acc ->
    let (hd, last) = (BatList.hd p.basic_path, BatList.last p.basic_path) in
    let func = FuncMap.find p.fkey global.fmap in
    let cfg = Func.cfg func in
    let comps_g = collect_comps_g global (ctx, p) in
    match ctx with
    | None ->
      let cands1 =
        if Func.(is_public func || is_external func) && is_external_call_node hd cfg then
          next_ext comps_g (Plain Lang.Node.extern) invmap
        else [] in
      let cands2 =
        if Func.(is_public func || is_external func) && is_external_call_node last cfg then
          next_ext comps_g (Plain Lang.Node.extern) invmap
        else [] in
      cands1 @ cands2 @ acc
    | Some ctx when ctx = Lang.Node.extern ->
      let cands =
        if Func.(is_public func || is_external func) && (Node.is_entry hd || Node.is_exit last) then
          next_ext comps_g (Plain ctx) invmap
        else [] in
      cands @ acc
    | _ -> acc
  ) paths []

(* make formulas to 'true' except for some specialized formulas *)
let rec special_only : vformula -> vformula
= fun vf ->
  match vf with
  | VAnd (f1,f2) -> VAnd (special_only f1, special_only f2)
  | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _
  | ForAll _ | VBinRel (VEq,Read (VVar ("@TU",_),_),_) -> vf
  | VOr _ | Imply _ -> assert false
  | _ -> VTrue

let rec make_locks_to_true : vformula -> vformula
= fun vf ->
  match vf with
  | VBinRel (VEq,VVar _,VCond VTrue)
  | VBinRel (VEq,VVar _,VCond VFalse) -> VTrue
  | VAnd (f1,f2) -> VAnd (make_locks_to_true f1, make_locks_to_true f2)
  | VOr _ | Imply _ -> assert false
  | _ -> vf

let rec make_locals_to_true : Global.t -> vformula -> vformula
= fun global vf ->
  match vf with
  | VAnd (f1,f2) -> VAnd (make_locals_to_true global f1, make_locals_to_true global f2)
  | VOr _ | Imply _ -> assert false
  | _ ->
    if BatSet.for_all (fun x -> List.mem x global.global_vars || List.mem (fst x) global_ghost_var_names) (free_vf vf) then vf
    else VTrue

let sync' all_cutpoints ext_lhs all_lhs target_lhs target_ext_lhs : Global.t -> InvMap.t -> InvMap.t
= fun global invmap ->
  let invmap = (* loop inv => tran inv (excluding local, locks) *)
    BatSet.fold (fun cp acc ->
      let loop_inv = InvMap.find cp acc in
      let trans_inv = InvMap.find (Plain Lang.Node.trans) acc in (* trans_inv may be updated at each iteration *)
      InvMap.add (Plain Node.trans) (VAnd (make_locals_to_true global (make_locks_to_true loop_inv), trans_inv)) acc
    ) all_lhs invmap in

  let invmap = (* tran inv => loop inv, extern inv, extern loop inv *)
    let trans_inv = InvMap.find (Plain Node.trans) invmap in
    BatSet.fold (fun cp acc ->
      InvMap.add cp (VAnd (trans_inv, InvMap.find cp invmap)) acc
    ) all_cutpoints invmap in

  let invmap = (* extern loop inv => extern inv *)
    if BatSet.is_empty global.extcalls then invmap
    else
      BatSet.fold (fun cp acc ->
        let ext_loopinv = InvMap.find cp invmap in
        let extern_inv = InvMap.find (Plain Node.extern) acc in
        InvMap.add (Plain Node.extern) (VAnd (extern_inv, ext_loopinv)) acc
      ) ext_lhs invmap in

  let invmap = (* extern inv => tran inv (excluding locks), extern loop inv *)
    if BatSet.is_empty global.extcalls then invmap
    else
      let extern_inv = InvMap.find (Plain Node.extern) invmap in
      let invmap = InvMap.add (Plain Node.trans)
                     (VAnd (make_locals_to_true global (special_only extern_inv), InvMap.find (Plain Node.trans) invmap))
                   invmap in
      BatSet.fold (fun cp acc ->
        InvMap.add cp (VAnd (extern_inv, InvMap.find cp invmap)) acc
      ) (BatSet.union ext_lhs target_lhs) invmap in

  let invmap = (* extern inv with locks => extern loop inv as false *)
    if BatSet.is_empty global.extcalls then invmap
    else
      let extern_inv = InvMap.find (Plain Node.extern) invmap in
      BatSet.fold (fun cp acc ->
        if contain_lock extern_inv then InvMap.add cp VFalse acc
        else acc
      ) target_ext_lhs invmap in
  invmap

(* let rec fix_invmap f invmap =
  let invmap' = InvMap.simplify (f invmap) in
  if InvMap.equal invmap' invmap then invmap'
  else fix_invmap f invmap' *)

let sync : Global.t -> InvMap.t -> InvMap.t list
= fun global invmap ->
  let lhs = BatSet.map (fun l -> Plain l) global.lhs_main in
  let lhs2 = BatSet.map (fun l -> Plain l) global.lhs2_main in (* TODO: loop headers whose functions contain external calls *)
  let lhs3 = BatSet.map (fun l -> Plain l) global.lhs3_main in

  let ext_lhs = if BatSet.is_empty global.extcalls then BatSet.empty else BatSet.map (fun l -> Ctx (Node.extern, l)) global.lhs_main in
  let ext_lhs2 = if BatSet.is_empty global.extcalls then BatSet.empty else BatSet.map (fun l -> Ctx (Node.extern, l)) global.lhs2_main in
  let ext_lhs3 = if BatSet.is_empty global.extcalls then BatSet.empty else BatSet.map (fun l -> Ctx (Node.extern, l)) global.lhs3_main in
  let all_cutpoints = if BatSet.is_empty global.extcalls then (assert (BatSet.is_empty ext_lhs); lhs)
                      else BatSet.add (Plain Node.extern) (BatSet.union ext_lhs lhs) in

  let invmap = InvMap.simplify invmap in
  let invmap1 = (sync' all_cutpoints ext_lhs lhs lhs ext_lhs global) invmap in
  let invmap2 = (sync' all_cutpoints ext_lhs lhs lhs2 ext_lhs2 global) invmap in
  let invmap3 = (sync' all_cutpoints ext_lhs lhs lhs3 ext_lhs3 global) invmap in

  let invmap1 = InvMap.simplify invmap1 in
  let invmap2 = InvMap.simplify invmap2 in
  let invmap3 = InvMap.simplify invmap3 in

  [invmap1; invmap2; invmap3]

let refine_spec : Global.t -> Path2.Set.t -> SpecMap.t -> SpecMap.t list
= fun global paths specmap ->
  Path2.Set.fold (fun (ctx, p) acc ->
    let comps_g = collect_comps_g global (ctx, p) in
    let callnodes = BatSet.map fst global.callnodes in
    let intersect = BatSet.intersect callnodes (BatSet.of_list p.basic_path) in
    BatSet.fold (fun n acc2 ->
      (next_spec comps_g n specmap) @ acc2
    ) intersect acc
  ) paths []

let next_spec : Global.t -> Path2.Set.t -> InvMap.t -> SpecMap.t -> (InvMap.t * SpecMap.t) list
= fun global _paths invmap _specmap ->
  let trans_inv = InvMap.find (Plain Node.trans) invmap in
  let specmap1 = BatSet.fold (fun (cn,_fk) acc -> SpecMap.add cn (trans_inv, trans_inv) acc) global.callnodes SpecMap.empty in
  let specmap2 =
    BatSet.fold (fun (cn,fk) acc ->
      let f = FuncMap.find fk global.fmap in
      if Global.contain_extern global f then
        let extern_inv = InvMap.find (Plain Node.extern) invmap in
        if not (contain_lock_s extern_inv f.body) then
          SpecMap.add cn (extern_inv, extern_inv) acc
        else
          SpecMap.add cn (trans_inv, trans_inv) acc
      else
        SpecMap.add cn (trans_inv, trans_inv) acc
    ) global.callnodes SpecMap.empty in
  let specmaps = [specmap1;specmap2] in
  List.map (fun specmap -> (invmap,specmap)) specmaps

(* generate refinements from problematic paths *)
let refine : Global.t -> Path2.Set.t -> InvMap.t * SpecMap.t -> (InvMap.t * SpecMap.t) list
= fun global paths (invmap,specmap) ->
  let lst =
    let tran = if !Options.intra then [] else refine_tran global paths invmap in
    let loop = refine_loop global paths invmap in
    let ext = refine_ext global paths invmap in
    tran @ loop @ ext in
  let lst = BatList.unique ~eq:InvMap.equal lst in
  let lst = BatList.fold_lefti (fun acc _ map -> acc @ (sync global map)) [] lst in
  (* let _ = List.iter (fun i -> print_endline (InvMap.to_string i)) lst in
  let _ = assert false in *)
  let final = List.fold_left (fun acc map -> acc @ (next_spec global paths map specmap)) [] lst in
  final
