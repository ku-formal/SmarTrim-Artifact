open Frontend
open Lang
open Vocab

type edge = Fkey.t * Fkey.t

let to_string_edge (k1,k2) = Fkey.to_string k1 ^ " -> " ^ Fkey.to_string k2
let to_string_edges edges = string_of_set ~first:"[" ~last:"]" ~sep:",\n" to_string_edge edges
let print_edges edges = print_endline (to_string_edges edges)

let edges_n ~ignore_extern : string list -> FuncMap.t -> Func.t -> Node.t -> edge BatSet.t -> edge BatSet.t
= fun cnames fmap curf node edges ->
  let stmt = Cfg.find_stmt node (Func.cfg curf) in
  match stmt with
  | Call (_,Lv (Var ("contract_init",_)),args,_,_,_) ->
    let cnstr_exp, cnstr_args = List.hd args, List.tl args in
    let _ = assert (List.mem (to_string_exp cnstr_exp) cnames) in
    let callees = FuncMap.find_matching_funcs (to_string_exp cnstr_exp) cnstr_exp (List.map get_type_exp cnstr_args) cnames fmap in
    let _ = assert (BatSet.cardinal callees = 1) in
    let callee = BatSet.choose callees in
    BatSet.add (Func.fkey curf, Func.fkey callee) edges
  | Call _ (* built-in functions *)
    when FuncMap.is_undef_call fmap stmt -> edges
  | Extcall _ ->
    if ignore_extern then edges
    else
      let funcs = List.map snd (BatMap.bindings fmap) in
      let funcs = List.filter (fun f -> f.Func.info.scope_s= !Options.main_contract) funcs in
      let funcs = List.filter (fun f -> Func.(is_public f || is_external f) && not (Func.is_constructor f)) funcs in
      List.fold_left (fun acc callee ->
        BatSet.add (Func.fkey curf, Func.fkey callee) acc
      ) edges funcs
  | Call (_,e,args,_,_,_) -> (* static or object calls *)
    let caller_cname = curf.Func.info.scope_s in
    let callees = FuncMap.find_matching_funcs caller_cname e (List.map get_type_exp args) cnames fmap in
    BatSet.fold (fun callee acc ->
      BatSet.add (Func.fkey curf, Func.fkey callee) acc
    ) callees edges
  | _ -> edges

let rec edges_s ~ignore_extern : string list -> FuncMap.t -> Func.t -> Stmt.t -> edge BatSet.t -> edge BatSet.t
= fun cnames fmap curf stmt edges ->
  match stmt with
  | Assign _ | Decl _ -> edges
  | Seq (s1,s2) -> edges |> edges_s ~ignore_extern cnames fmap curf s1 |> edges_s ~ignore_extern cnames fmap curf s2
  | Call _ (* built-in functions *)
    when FuncMap.is_undef_call fmap stmt -> edges
  | Extcall _ ->
    if ignore_extern then edges
    else
      let funcs = List.map snd (BatMap.bindings fmap) in
      let funcs = List.filter (fun f -> f.Func.info.scope_s= !Options.main_contract) funcs in
      let funcs = List.filter (fun f -> Func.(is_public f || is_external f) && not (Func.is_constructor f)) funcs in
      List.fold_left (fun acc callee ->
        BatSet.add (Func.fkey curf, Func.fkey callee) acc
      ) edges funcs
  | Call (_,e,args,_,_,_) -> (* static or object calls *)
    let caller_cname = curf.Func.info.scope_s in
    let callees = FuncMap.find_matching_funcs caller_cname e (List.map get_type_exp args) cnames fmap in
    BatSet.fold (fun callee acc ->
      BatSet.add (Func.fkey curf, Func.fkey callee) acc
    ) callees edges
  | Skip -> edges
  | If (_,s1,s2,_) -> edges |> edges_s ~ignore_extern cnames fmap curf s1 |> edges_s ~ignore_extern cnames fmap curf s2
  | While (_,s) -> edges_s ~ignore_extern cnames fmap curf s edges
  | Break | Continue | Return _
  | Revert | Assume _ | Assert _
  | Assembly _ | Placeholder -> edges
  | Unchecked (lst,_) -> list_fold (edges_s ~ignore_extern cnames fmap curf) lst edges
  | Label _ -> edges

let edges_f ~ignore_extern ?(inlined_cfg=true) : string list -> FuncMap.t -> Func.t -> edge BatSet.t -> edge BatSet.t
= fun cnames fmap f edges ->
  if inlined_cfg then
    let nodes = Cfg.nodes_of f.info.cfg in
    list_fold (edges_n ~ignore_extern cnames fmap f) nodes edges
  else
    edges_s ~ignore_extern cnames fmap f f.body edges

let edges_c ~ignore_extern ?(inlined_cfg=true) : string list -> FuncMap.t -> Contract.t -> edge BatSet.t -> edge BatSet.t
= fun cnames fmap c edges ->
  list_fold (edges_f ~ignore_extern ~inlined_cfg cnames fmap) c.funcs edges

let edges_p ~ignore_extern ?(inlined_cfg=true) : string list -> FuncMap.t -> Pgm.t -> edge BatSet.t
= fun cnames fmap p ->
  list_fold (edges_c ~ignore_extern ~inlined_cfg cnames fmap) p BatSet.empty

let collect_call_edges ~ignore_extern ?(inlined_cfg=true) : string list -> FuncMap.t -> Pgm.t -> edge BatSet.t
= fun cnames fmap p ->
  edges_p ~ignore_extern ~inlined_cfg cnames fmap p

let init_callees : Pgm.t -> Fkey.t BatSet.t
= fun p ->
  let main = Pgm.main p in
  let funcs = List.filter (fun f -> Func.is_public f || Func.is_external f || Func.is_constructor f) main.funcs in
  BatSet.of_list (List.map Func.fkey funcs)

let onestep_callees : Fkey.t BatSet.t -> edge BatSet.t -> Fkey.t BatSet.t
= fun callees edges ->
  BatSet.fold (fun (k1,k2) acc ->
    if BatSet.mem k1 callees then BatSet.add k2 acc
    else acc
  ) edges callees

let onestep_callers : Fkey.t BatSet.t -> edge BatSet.t -> Fkey.t BatSet.t
= fun callers edges ->
  BatSet.fold (fun (k1,k2) acc ->
    if BatSet.mem k2 callers then BatSet.add k1 acc
    else acc
  ) edges callers

let rec fix f : Fkey.t BatSet.t -> edge BatSet.t -> Fkey.t BatSet.t
= fun fkeys edges ->
  let next = f fkeys edges in
  if BatSet.subset next fkeys then next
  else fix f next edges

let transitive_callees : Fkey.t BatSet.t -> edge BatSet.t -> Fkey.t BatSet.t
= fun init edges -> fix onestep_callees init edges

let transitive_callers : Fkey.t BatSet.t -> edge BatSet.t -> Fkey.t BatSet.t
= fun init edges -> fix onestep_callers init edges

let compute_reachable_funcs : string list -> FuncMap.t -> Pgm.t -> Fkey.t BatSet.t
= fun cnames fmap p -> 
  transitive_callees (init_callees p) (collect_call_edges ~ignore_extern:true cnames fmap p)

let rm_unreach_c (reachable : Fkey.t BatSet.t) c =
  Field.map Contract.Fields.funcs ~f:(List.filter (fun f -> BatSet.mem (Func.fkey f) reachable)) c

let remove_unreachable_funcs ?(silent=false) (p : Pgm.t) : Pgm.t =
  let cnames = Pgm.cnames p in
  let fmap = FuncMap.mk_fmap p in
  let all = get_all_fkeys p in
  let reachable = compute_reachable_funcs cnames fmap p in
  let p' = List.map (rm_unreach_c reachable) p in
  if not silent then print_endline ("[INFO] all functions : " ^ string_of_int (BatSet.cardinal all));
  if not silent then print_endline ("[INFO] reachable functions : " ^ string_of_int (BatSet.cardinal reachable));
  if !Options.debug = "log" then prerr_endline (string_of_set Fkey.to_string ~sep:", " reachable);
  p'
