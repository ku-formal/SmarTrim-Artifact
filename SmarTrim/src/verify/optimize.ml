open Frontend
open Vlang

(** compute all vars that may be control- or data- dependent w.r.t. given vars. *)
let rec collect : vformula -> Var.t BatSet.t -> Var.t BatSet.t 
= fun vf vars ->
  match vf with
  | VTrue | VFalse -> vars 
  | VNot f -> collect f vars
  | VAnd (f1,f2) -> BatSet.union (collect f1 vars) (collect f2 vars) 
  | VOr (f1,f2) -> BatSet.union (collect f1 vars) (collect f2 vars)
  | VBinRel (_,e1,e2) ->
    let vars1, vars2 = free_ve e1, free_ve e2 in
    let union = BatSet.union vars1 vars2 in
    if not (BatSet.disjoint union vars) then BatSet.union union vars
    else vars
  | Imply (f1,f2) -> BatSet.union (collect f1 vars) (collect f2 vars)
  | ForAll (bvs,f) -> BatSet.diff (collect f vars) (BatSet.of_list bvs)
  | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ ->
    let vars' = free_vf vf in
    if not (BatSet.disjoint vars vars') then BatSet.union vars' vars
    else vars
  | Label (Assign_info `Assign, VBinRel (VEq,VVar x,e)) ->
    if BatSet.mem x vars then BatSet.union (free_ve e) vars
    else vars
  | Label (Assign_info `Assign, VTrue) ->
    (* this case may appear when constraints are invalidated by
     * assembly block or function calls that are not inlined. *)
    vars
  | Label (Assign_info `Assign, _) -> failwith ("collect (Label0) : " ^ to_string_vformula vf)
  | Label (_, f) -> collect f vars

let rec fix collect vf vars =
  let vars' = collect vf vars in
  if BatSet.subset vars' vars then vars'
  else fix collect vf vars'

let rec pc_vars_vf : bool -> vformula -> Var.t BatSet.t
= fun is_pc vf ->
  match vf with
  | VTrue | VFalse -> BatSet.empty
  | VNot f -> pc_vars_vf is_pc f
  | VAnd (f1,f2) -> BatSet.union (pc_vars_vf is_pc f1) (pc_vars_vf is_pc f2)
  | VOr (f1,f2) -> BatSet.union (pc_vars_vf is_pc f1) (pc_vars_vf is_pc f2)
  | VBinRel (_,e1,e2) ->
    if is_pc then BatSet.union (free_ve e1) (free_ve e2)
    else BatSet.empty
  | SigmaEqual _ | NoOverflow _ | UntrustSum _ | UntrustSum2 _ -> BatSet.empty
  | Imply (f1,f2) -> (* failwith ("pc_vars_vf (Imply) : " ^ to_string_vformula vf) *)
    assert (!Options.mode = Verify);
    pc_vars_vf is_pc (VOr (VNot f1, f2))
  | ForAll (bvs,f) -> BatSet.diff (pc_vars_vf is_pc f) (BatSet.of_list bvs)
  | Label (s,f) ->
    if Label.equal s (Assign_info `Assign) then BatSet.empty
    else if Label.equal s (Assign_info `Assume) then pc_vars_vf true f
    else failwith ("pc_vars_vf (Label) : " ^ to_string_vformula vf)

let compute_dependant_vars : vformula -> vformula -> Var.t BatSet.t
= fun pgm_const safety ->
  let init = BatSet.union (free_vf safety) (pc_vars_vf false pgm_const) in
  fix collect pgm_const init

let rec rep_true : Var.t BatSet.t -> vformula -> vformula
= fun vars vf ->
  match vf with
  | VTrue | VFalse -> vf
  | VNot _ ->
    if not (BatSet.subset (free_vf vf) vars) then VTrue
    else vf
  | VAnd (f1,f2) -> VAnd (rep_true vars f1, rep_true vars f2)
  | VOr (f1,f2) -> VOr (rep_true vars f1, rep_true vars f2)
  | VBinRel _ ->
    if not (BatSet.subset (free_vf vf) vars) then VTrue
    else vf
  | Imply _ -> failwith "rep_true"
  | ForAll (bvs,f) ->
    let fvs = BatSet.diff (free_vf f) (BatSet.of_list bvs) in
    if not (BatSet.subset fvs vars) then VTrue
    else vf
  | SigmaEqual (x,e) ->
    let union = BatSet.add x (free_ve e) in
    if not (BatSet.subset union vars) then VTrue
    else vf
  | NoOverflow x ->
    if not (BatSet.mem x vars) then VTrue
    else vf
  | UntrustSum (v1,v2) ->
    let set = BatSet.of_list [v1;v2] in
    if not (BatSet.subset set vars) then VTrue
    else vf
  | UntrustSum2 (v1,v2,v3) ->
    let set = BatSet.of_list [v1;v2;v3] in
    if not (BatSet.subset set vars) then VTrue
    else vf
  | Label (_,f) -> rep_true vars f

let remove_unrelated_part : Var.t BatSet.t -> vformula -> vformula
= fun vars vf -> rep_true vars vf
