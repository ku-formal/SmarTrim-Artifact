open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open MakeCfg

let may_violate_cei = ref false
let may_call_itself = ref false

let may_update_state_f (global : Global.t) f =
  let defs = FuncDefUse.find_d (Func.fkey f) global.f_defuse in
  let field_names = Global.get_all_fields global |> List.map fst in
  Func.is_payable f 
  (* TODO: in the current implementation, increasing "Invest_sum" is modelled in semantics model, rather than preprocessing step *)
  || BatSet.exists (fun d -> List.mem d (List.map fst global.global_vars) || d = "@Invest_sum" || List.mem d field_names) defs

let is_state_manipulating_assign global (stmt : Stmt.t) : bool =
  let fields = Global.get_all_fields global in
  let field_names = List.map fst fields in
  match stmt with
  | Assign (lv,_,_) ->
    let defs = FuncDefUse.get_def_set_lv lv in
    BatSet.exists (fun d -> List.mem d (List.map fst global.global_vars) || d = "@Invest_sum" || List.mem d field_names) defs
  | _ -> false

let may_update_state : Global.t -> Func.t -> Node.t -> bool
= fun global curf n ->
  let g = Func.cfg curf in
  let stmt = find_stmt n g in
  match stmt with
  | _ when is_state_manipulating_assign global stmt -> true
  | Call (lvop,_,_,_,_,_) when is_undef_call global.fmap stmt ->
    (match lvop with
     | None -> false
     | Some lv ->
       let fields = Global.get_all_fields global in
       let field_names = List.map fst fields in
       let defs = FuncDefUse.get_def_set_lv lv in
       BatSet.exists (fun d -> List.mem d (List.map fst global.global_vars) || d = "@Invest_sum" || List.mem d field_names) defs)
  | Call _ when is_internal_call global.fmap global.cnames stmt ->
    assert (no_eth_gas_modifiers stmt);
    let callee = Global.get_callee_of_intcall global curf stmt in
    may_update_state_f global callee
  | _ -> false

let callee_contain_extcall : Global.t -> Func.t -> Node.t -> Cfg.t -> bool
= fun global f n g ->
  let callee = Global.get_callee_of_intcall global f (find_stmt n g) in
  Global.contain_extern global callee

(*
let get_ext_nodes : Global.t -> Path.t -> int list
= fun global (fk,nodes) ->
  let f = FuncMap.find fk global.fmap in
  let g = get_cfg f in
  BatList.fold_lefti (fun acc i n ->
    if is_external_call_node n g then acc @ [i]
    else if is_internal_call_node global.fmap global.cnames n g then
      if callee_contain_extcall global f n g then acc @ [i]
      else acc
    else acc
  ) [] nodes

let get_state_nodes : Global.t -> Path.t -> int list
= fun global (fk,nodes) ->
  let f = FuncMap.find fk global.fmap in
  BatList.fold_lefti (fun acc i n ->
    if may_update_state global f n then acc @ [i]
    else acc
  ) [] nodes

let debug_cei_hold' path s e =
  if s >= e then
    (print_endline (Path.to_string path);
    print_endline (string_of_int e);
    print_endline (string_of_int s);
    print_endline "")

let cei_hold' : Global.t -> Path.t -> bool
= fun global path ->
  let ext_nodes = get_ext_nodes global path in
  let state_nodes = get_state_nodes global path in
  List.for_all (fun e ->
    List.for_all (fun s ->
      (* let _ = debug_cei_hold' path s e in *)
      (* The case 's=e' exists when function call containing external calls exists, e.g., 'transferFrom' in 2018-13128  *)
      s <= e) state_nodes
  ) ext_nodes

(* check if cei pattern holds *)
let cei_hold : Global.t -> PathSet.t -> bool
= fun global paths -> PathSet.for_all (cei_hold' global) paths
*)

let is_ext_node : Global.t -> Func.t -> Node.t -> bool
= fun global f n ->
  let g = Func.cfg f in
  if is_external_call_node n g then true
  else if is_internal_call_node global.fmap global.cnames n g then
    if callee_contain_extcall global f n g then true
    else false
  else false

let get_ext_nodes global f =
  let nodes = nodesof (Func.cfg f) in
  List.filter (is_ext_node global f) nodes

let get_all_succs g node =
  let onestep g nodes = BatSet.fold (fun n acc -> BatSet.union (BatSet.of_list (succ n g)) acc) nodes nodes in
  BatSet.diff (Vocab.fix (onestep g) (BatSet.singleton node)) (BatSet.singleton node)

let cei_hold' global func =
  let cfg = Func.cfg func in
  let ext_nodes = get_ext_nodes global func in
  List.for_all (fun e ->
    let all_succs = BatSet.to_list (get_all_succs cfg e) in
    List.for_all (fun succ ->
      not (may_update_state global func succ)
      (* let b = not (may_update_state global func succ) in
      let _ = if not b then print_endline (get_fname func ^ "  : " ^ Node.to_string e ^ " -> " ^ Node.to_string succ) in
      b *)
    ) all_succs
  ) ext_nodes

(* check if cei pattern holds *)
let cei_hold : Global.t -> Func.t list -> bool
= fun global funcs -> List.for_all (cei_hold' global) funcs

let check_recursive'' : Global.t -> Cfg.t -> Node.t -> bool
= fun global g node ->
  let stmt = find_stmt node g in
  match stmt with
    (* built-in functions *)
  | Call _ when FuncMap.is_undef_call global.fmap stmt -> false
  | Call _ when is_internal_call global.fmap global.cnames stmt -> false
  | Call (_,e,_,_,_,_) ->
    let rcv = match e with Lv (MemberAccess (rcv,_,_,_)) -> rcv | _ -> assert false in
    (match get_type_exp rcv with
     | EType (Contract c) -> List.mem c global.base_names
     | _ -> assert false)
  | _ -> false

let check_recursive' : Global.t -> Paths.t -> bool
= fun global p ->
  let f = FuncMap.find p.fkey global.fmap in
  let g = Func.cfg f in
  List.exists (check_recursive'' global g) p.basic_path

let check_recursive global paths = Paths.Set.exists (check_recursive' global) paths

let run global pgm paths =
  may_call_itself := check_recursive global paths;
  may_violate_cei := not (cei_hold global (pgm |> Pgm.main |> Contract.funcs));
  print_endline ("[INFO] Violate CEI: " ^ string_of_bool !may_violate_cei);
  print_endline ("[INFO] msg.sender = this possible: " ^ string_of_bool !may_call_itself);
  ()
