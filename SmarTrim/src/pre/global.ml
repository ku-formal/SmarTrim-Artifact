open Frontend
open Frontend.Lang
open MakeCfg
open! Vocab
open Vocab.Batteries
include Global_intf

(*************************)
(*************************)
(*** Some useful utils ***)
(*************************)
(*************************)

let contain_line (line : int) (func : Func.t) : bool =
  let finfo = func.info in
  finfo.floc.line <= line && line <= finfo.floc.finish_line
;;

let find_func : int -> Func.t list -> Func.t = fun line funcs -> List.find (contain_line line) funcs

let find_func_containing_line (line : int) (global : t) : Func.t =
  let all_funcs = List.fold_left (fun acc c -> acc @ Contract.funcs c) [] global.pgm0 in
  find_func line all_funcs
;;

let get_all_fields (global : t) : Var.t list =
  let field_lst = Map.bindings global.struct_map |> List.map snd in
  let fields =
    List.fold_left (fun acc members -> acc @ List.map Var2.to_var members) [] field_lst
  in
  fields
;;

let get_callee_of_intcall (global : t) (func : Func.t) (stmt : Stmt.t) : Func.t =
  let caller_cname = func.info.scope_s in
  match stmt with
  | Call (_lvop, e, args, _ethop, _gasop, _loc) ->
    let callees =
      FuncMap.find_matching_funcs caller_cname e (List.map get_type_exp args) global.cnames
        global.fmap
    in
    assert (Set.cardinal callees = 1);
    Set.choose callees
  | _ -> assert false
;;

let is_intcall_from_other_contracts (stmt : Stmt.t) =
  match stmt with
  (* could be function calls using 'super' keywords. *)
  (* E.g, "super.withdraw" in reentrancy/0x60C6b5DC066E33801F2D9F2830595490A3086B4e_1.sol *)
  | Call (_, Lv (MemberAccess (Lv (Var (x, _)), _, _, _)), _, _, _, _) ->
    not (x = !Options.main_contract)
  | Call _ -> false
  | _ -> assert false
;;

(** internal/priviate, view/pure functions, and functions from other contracts are considered to be
    harmless in terms of deadcode, as they will not be annotated with NR *)
let is_live_intcall callee stmt =
  Func.is_internal callee || Func.is_private callee || Func.is_view_pure callee
  || is_intcall_from_other_contracts stmt
;;

let rec contain_dead_intcall' global f (stmt : Stmt.t) : bool =
  match stmt with
  | Assign _ | Decl _ -> false
  | Seq (s1, s2) -> contain_dead_intcall' global f s1 || contain_dead_intcall' global f s2
  | Call _ when FuncMap.is_undef_call global.fmap stmt -> false
  | Call _ when FuncMap.is_internal_call global.fmap global.cnames stmt ->
    let callee = get_callee_of_intcall global f stmt in
    not (is_live_intcall callee stmt)
  | Call _ -> false
  | Extcall _ -> false
  | Skip -> false
  | If (_e, s1, s2, _i) -> contain_dead_intcall' global f s1 || contain_dead_intcall' global f s2
  | While (_e, s) -> contain_dead_intcall' global f s
  | Break | Continue | Return _ | Revert | Assume _ | Assert _ | Assembly _ | Label _ -> false
  | Placeholder -> false (* could be included in modifier that contain function calls *)
  | Unchecked (slst, _) -> List.exists (contain_dead_intcall' global f) slst
;;

(** works at AST-level *)
let contain_dead_intcall (global : t) (f : Func.t) : bool =
  (* consider transitive closure : reentrancy/0xd5500f15abdab81f4ede3a30a215964a4068a4b7_1 *)
  (* purchaseToken (public) -> t_transfer (internal) -> transferFrom (public): could incur deadcode if NR is applied to 'pruchaseToken' and 'transferFrom' *)
  let reachable = CallGraph.transitive_callees (Set.singleton (Func.fkey f)) global.call_edges in
  let reachable = Set.map (fun k -> FuncMap.find k global.fmap) reachable in
  Set.exists (fun f' -> contain_dead_intcall' global f' (Func.body f')) reachable
;;

let contain_dead_intcall_cfg' (global : t) (f : Func.t) : bool =
  let cfg = Func.cfg f in
  let nodes = nodesof cfg in
  let nodes =
    List.filter (fun n -> FuncMap.is_internal_call_node global.fmap global.cnames n cfg) nodes
  in
  List.exists
    (fun n ->
      let callee = get_callee_of_intcall global f (find_stmt n cfg) in
      not (is_live_intcall callee (find_stmt n cfg)))
    nodes
;;

let contain_dead_intcall_cfg : t -> Func.t -> bool =
 fun global f ->
  let reachable = CallGraph.transitive_callees (Set.singleton (Func.fkey f)) global.call_edges in
  let reachable = Set.map (fun k -> FuncMap.find k global.fmap) reachable in
  Set.exists (fun f' -> contain_dead_intcall_cfg' global f') reachable
;;

let rec contain_extern' (stmt : Stmt.t) =
  match stmt with
  | Extcall _ -> true
  | Assign _ | Decl _ | Call _ | Skip | Break | Continue | Return _ | Revert | Assume _ | Assert _
  | Assembly _ | Label _ ->
    false
  | Seq (s1, s2) -> contain_extern' s1 || contain_extern' s2
  | If (_e, s1, s2, _i) -> contain_extern' s1 || contain_extern' s2
  | While (_e, s) -> contain_extern' s
  | Placeholder -> assert false
  | Unchecked (slst, _) -> List.exists contain_extern' slst
;;

let contain_extern (global : t) (f : Func.t) : bool =
  let reachable = CallGraph.transitive_callees (Set.singleton (Func.fkey f)) global.call_edges in
  Set.exists (fun k -> contain_extern' (Func.body (FuncMap.find k global.fmap))) reachable
;;

let is_constant_address (global : t) (var : Var.t) : bool =
  assert (Typ.is_address_kind (get_type_var var));
  let itv = ItvDom.Val.itv_of (ItvDom.Mem.find var global.mem) in
  if Itv.is_top itv || Itv.is_bot itv then false
  else match itv with Itv (V l, V u) when Z.equal l u -> true | _ -> false
;;

(************************************)
(*** Collect hard-coded addresses ***)
(************************************)

let rec hc_lv lv =
  match lv with
  | Var _ -> Z.Set.empty
  | MemberAccess (e, _, _, _) -> hc_exp e
  | IndexAccess (e, None, _) -> hc_exp e
  | IndexAccess (e1, Some e2, _) -> Z.Set.union (hc_exp e1) (hc_exp e2)
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop -> match eop with None -> acc | Some e -> Z.Set.union (hc_exp e) acc)
      Z.Set.empty eops

and hc_exp exp =
  match exp with
  | V _ | Str _ -> Z.Set.empty
  | Lv lv -> hc_lv lv
  | Cast (EType Address, V (Int n)) when not (Z.equal n Z.zero) -> Z.Set.singleton n
  | Cast (_, e) -> hc_exp e
  | BinOp (_, e1, e2, _) -> Z.Set.union (hc_exp e1) (hc_exp e2)
  | UnOp (_, e, _) -> hc_exp e
  | Ite (i, t, e, _) -> Z.Set.union (Z.Set.union (hc_exp i) (hc_exp t)) (hc_exp e)
  | ETypeName _ -> Z.Set.empty
  | IndexRangeAccess (base, startop, finop, _) ->
    let set1 = hc_lv base in
    let set2 = match startop with Some e -> hc_exp e | None -> Z.Set.empty in
    let set3 = match finop with Some e -> hc_exp e | None -> Z.Set.empty in
    Z.Set.union set1 (Z.Set.union set2 set3)
  | TypeInfo _ -> Z.Set.empty
  | _ -> failwith "hc_exp: temp expressions encountered"
;;

let rec hca_s (stmt : Stmt.t) : Z.Set.t =
  let is_address_array typ = match typ with Typ.Array (EType Address, _) -> true | _ -> false in
  match stmt with
  | Assign (lv, V (Int n), _) when Typ.is_address (get_type_lv lv) && not (Z.equal n Z.zero) ->
    Z.Set.singleton n
  | Assign (lv, Lv (Tuple (eops, _)), _) when is_address_array (get_type_lv lv) ->
    (* e.g., cve-2018-14088 *)
    List.fold_left
      (fun acc eop -> match eop with Some exp -> Z.Set.union acc (hc_exp exp) | None -> acc)
      Z.Set.empty eops
  | Assign (lv, e, _) -> Z.Set.union (hc_lv lv) (hc_exp e)
  | Decl _ -> Z.Set.empty
  | Assume (e, _) -> hc_exp e
  | Assert (e, _, _) -> hc_exp e
  | Call (lvop, _, args, _, _, _) ->
    let hc1 = match lvop with None -> Z.Set.empty | Some lv -> hc_lv lv in
    let hc2 = List.fold_left (fun acc arg -> Z.Set.union (hc_exp arg) acc) Z.Set.empty args in
    Z.Set.union hc1 hc2
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (id, fkey, ether, gas, is_static, loc);
    let hc1 = match ret with None -> Z.Set.empty | Some lv -> hc_lv lv in
    let hc2 = List.fold_left (fun acc arg -> Z.Set.union (hc_exp arg) acc) Z.Set.empty args in
    let hc3 = hc_exp target in
    Z.Set.union hc1 hc2 |> Z.Set.union hc3
  | Seq (s1, s2) -> Z.Set.union (hca_s s1) (hca_s s2)
  | Skip -> Z.Set.empty
  | If (e, s1, s2, _) -> Z.Set.union (hc_exp e) (Z.Set.union (hca_s s1) (hca_s s2))
  | While (e, s) -> Z.Set.union (hc_exp e) (hca_s s)
  | Break | Continue -> Z.Set.empty
  | Return (eop, _) -> ( match eop with None -> Z.Set.empty | Some e -> hc_exp e)
  | Revert | Assembly _ | Placeholder -> Z.Set.empty
  | Unchecked (lst, _) -> List.fold_left (fun acc s -> Z.Set.union (hca_s s) acc) Z.Set.empty lst
  | Label _ -> Z.Set.empty
;;

let hca_f (f : Func.t) = hca_s f.body

let hca_c (c : Contract.t) : Z.Set.t =
  List.fold_left (fun acc f -> Z.Set.union (hca_f f) acc) Z.Set.empty c.funcs
;;

let collect_hc_addrs (pgm : Pgm.t) : Z.Set.t =
  List.fold_left (fun acc c -> Z.Set.union (hca_c c) acc) Z.Set.empty pgm
;;

let get_basenames pgm =
  let main_c = Pgm.main pgm in
  let bases = main_c.info.inherit_order in
  (* left: main (child), right: parent *)
  let bases = List.map (Pgm.find_nid pgm) bases in
  let bases_wo_interface = List.filter Contract.is_contract bases in
  let bases, bases_wo_interface =
    (List.map Contract.name bases, List.map Contract.name bases_wo_interface)
  in
  (bases, bases_wo_interface)
;;

let[@warning "-32"] get_base_contracts pgm =
  let main_c = Pgm.main pgm in
  let bases = main_c.info.inherit_order in
  (* left: main (child), right: parent *)
  let bases = List.map (Pgm.find_nid pgm) bases in
  let bases_wo_interface = List.filter Contract.is_contract bases in
  (bases, bases_wo_interface)
;;

let[@warning "-32"] get_full_basenames pgm (c : Contract.t) =
  let bases = c.info.inherit_order in
  (* left: main (child), right: parent *)
  let bases = List.map (Pgm.find_nid pgm) bases in
  let bases = List.map Contract.name bases in
  bases
;;

let mk_seed (flines : string list) : int array =
  let ctx = Digestif.KECCAK_256.init () in
  let ctx = List.fold_left (fun ctx s -> Digestif.KECCAK_256.feed_string ctx s) ctx flines in
  let t = Digestif.KECCAK_256.get ctx in
  let hex = Digestif.KECCAK_256.to_hex t in
  let hex = "0x" ^ hex in
  let z = Z.of_string hex in
  let m = Z.(of_int 2 ** 32) in
  let rec z_to_int_list (acc : int list) (z : Z.t) : int list =
    if Z.(equal z zero) then acc
    else
      let a, b = (Z.(div z m), Z.(rem z m)) in
      z_to_int_list (Z.to_int b :: acc) a
  in
  let l = z_to_int_list [] z in
  Array.of_list l
;;

let make_global_info (p : Pgm.t) (flines : string list) : t =
  let cnames = Pgm.cnames p in
  let gvars = Pgm.gvars p in
  let hardcoded_addrs = collect_hc_addrs p in
  let smap = StructMap.mk_smap p in
  let enums = (Pgm.main p).enums in
  let fmap = FuncMap.mk_fmap p in
  let defuse = FuncDefUse.compute cnames fmap in
  let call_edges =
    CallGraph.collect_call_edges ~ignore_extern:true ~inlined_cfg:false cnames fmap p
  in
  let bases, bases_wo_interface = get_basenames p in
  let extcall_locations =
    let fs = List.map Contract.funcs p in
    let exts = List.map (List.map (Field.get Func.Fields.extcall_locations)) fs in
    let exts = List.concat exts in
    let exts = List.concat exts in
    let exts = List.map (fun (e : Extcall.t) -> (e.id, e)) exts in
    Map.Int.of_list exts
  in
  let struct_handler = Struct_handler.mk p in
  Options.struct_bit_usage := Struct_handler.bit_usage struct_handler;
  let e = Set.empty in
  {
    pgm = p;
    (* will be updated after removing unreachables *)
    pgm0 = p;
    flines;
    cnames;
    hardcoded_addrs;
    seed = mk_seed flines;
    global_vars = gvars;
    mem = ItvDom.Mem.bot;
    struct_map = smap;
    enums;
    fmap;
    f_defuse = defuse;
    lhs = e;
    lhs2 = e;
    lhs3 = e;
    lhs_main = e;
    lhs2_main = e;
    lhs3_main = e;
    callnodes = e;
    callnodes_main = e;
    extcalls = e;
    extcall_locations;
    call_edges;
    base_names = bases;
    base_names_wo_interface = bases_wo_interface;
    struct_handler;
  }
;;
