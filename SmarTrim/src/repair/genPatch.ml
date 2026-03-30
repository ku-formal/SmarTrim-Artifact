open Frontend
open Frontend.Lang
open Vocab
open FixTarget
open Patch
open Patch.PatchComp
open GenPatchUtil

let repair_each : Global.t -> Pgm.t -> fix_target -> patch_comp list -> patch_comp list
= fun global pgm (kind,fkey_str,line,e) acc ->
  (* (let f = FuncMap.find fkey global.fmap in *)
  (match kind with
  | IO ->
    if fkey_str = "none" then [] (* cve-2018-14004, line 85 / 2018-13519, line 178 *)
    else
      let f = Global.find_func_containing_line line global in
      let ae = match e with None -> assert false | Some ae -> ae in
      PatchIO.generate line ae (collect_exp f.body)
  | RE_EL | RE -> PatchRE.generate global (Pgm.main pgm).funcs
  | EL ->
    let f = Global.find_func_containing_line line global in
    PatchEL.generate global pgm f line
  | SU ->
    let f = Global.find_func_containing_line line global in
    PatchSU.generate global pgm f
  | TX_ORG ->
    let f = Global.find_func_containing_line line global in
    PatchTX.generate global pgm f
  | _ -> raise NotImplemented)
  |> (fun lst -> List.sort_uniq PatchComp.compare (lst@acc))

let extract_patch_comps : Global.t -> Pgm.t -> fix_target BatSet.t -> patch_comp list
= fun global pgm fix_targets ->
  (let base = BatSet.fold (repair_each global pgm) fix_targets [] in
  if BatSet.exists Kind.(fun (k,_,_,_) -> k = EL || k = SU) fix_targets then
    base @ (PatchACC.report_unaware_template global pgm)
  else base)
  |> List.sort_uniq PatchComp.compare

let contain_move_nr_both : patch -> bool
= fun patch ->
  List.exists PatchComp.contain_move patch
  && List.exists PatchComp.contain_nr patch
  (* let res = List.exists contain_move patch && List.exists contain_nr patch in
  let _ = if res then print_endline (to_string_patches patch) in
  res *)

let unuseful_patterns2 : patch -> bool
= fun patch ->
  let indexed_patch = List.mapi (fun i c -> (i,c)) patch in
  let product = BatList.cartesian_product indexed_patch indexed_patch in
  List.exists (fun ((i,c1),(j,c1')) ->
    match c1,c1' with
    (* allow different insertions (e.g., different guards) at the same lines *)
    | Atom (InsertLine (_,_,_)), Atom (InsertLine (_,_,_)) when not (i = j) && not (PatchComp.compare c1 c1' = 0) -> false

    (* Prune 1. a single patch cannot have multiple NR templates. *)
    | _ when not (i = j) && contain_nr c1 && contain_nr c1' -> true

    (* This case is subsumed by the 'Prune 2', but left for clarity. *)
    (* E.g., insert (4, a+b>=a), insert (4, a+b>=a) *)
    | _ when not (i = j) && PatchComp.compare c1 c1' = 0 -> true

    (* Prune 2. a patch cannot have components that mutate overlapping lines *)
    (* E.g., Replace (5, a+b>a, a+b<a), Replace (5, a+b>a, a+b<=a) *)
    | _ when not (i = j) && not (BatSet.disjoint (PatchComp.lines_of c1) (PatchComp.lines_of c1')) ->
      true
    | _ -> false
  ) product

let likely_unuseful : patch -> bool
= fun patch ->
  contain_move_nr_both patch
  || unuseful_patterns2 patch

let gen_candidates : patch_comp list -> patch -> patch list
= fun components patch ->
  List.fold_left (fun acc comp ->
    if not (likely_unuseful (patch @ [comp])) then acc @ [patch @ [comp]]
    else acc
  ) [] components
