open Patch
open Patch.PatchComp
open! Frontend
open Frontend.Lang
open GenPatchUtil

(* leak/0x0208a56ea43c3ff6c2611071948794c2674a16b9_3 (transferOwnership) *)
(* leak/0x0d0edbcf2c0da25ac9a79c9466989e46a8f96221_2 (withdrawFunds) *)
(* leak/0x2120887e9e4889653770b228ee8d13231d0ef045_2 (transferAnything) *)

let likely_by_all' : Global.t -> Func.t -> bool
= fun global func ->
  let g = Func.cfg func in
  let nodes = MakeCfg.nodesof g in
  let gnames = List.map fst global.global_vars in
  List.exists (fun n ->
    let stmt = Cfg.find_stmt n g in
    match stmt with
    | Assign ((IndexAccess _) as lv, _, _) when Typ.is_uintkind (get_type_lv lv) ->
      let defs = FuncDefUse.get_def_set_lv lv in
      BatSet.exists (fun d -> List.mem d gnames) defs
    | Assign (MemberAccess (_,"balance",_,_),_,_) -> false
    | Assign ((MemberAccess _) as lv, _, _) -> Typ.is_uintkind (get_type_lv lv) || Typ.is_enum (get_type_lv lv)
      (* removed due to the case in reentrancy_bonus.sol *)
      (* BatSet.mem Vlang.msg_sender (var_lv lv) && is_uintkind (get_type_lv lv) *)
    | _ -> false
  ) nodes

let exist_def_addr (global : Global.t) func =
  let g = Func.cfg func in
  let nodes = MakeCfg.nodesof g in
  let gnames = List.map fst global.global_vars in
  List.exists (fun n ->
    let stmt = Cfg.find_stmt n g in
    match stmt with
    | Assign (lv, _, _) when Typ.is_address_kind (get_type_lv lv) ->
      let defs = FuncDefUse.get_def_set_lv lv in
      BatSet.exists (fun d -> List.mem d gnames) defs
    | _ -> false
  ) nodes

let rec find_eth_send global (stmt : Stmt.t) =
  match stmt with
  | Call (_,Lv (MemberAccess (e,fname,_,_)),_,_,_,_)
    when Typ.is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] -> true
  | Seq (s1,s2) -> find_eth_send global s1 || find_eth_send global s2
  | If (_,s1,_,_) -> find_eth_send global s1
  | _ -> false

let rec eth_send_in_loop' global (stmt : Stmt.t) =
  match stmt with
  | Seq (s1,s2) -> eth_send_in_loop' global s1 || eth_send_in_loop' global s2
  | While (_,s) -> find_eth_send global s
  | If _ -> false
  | _ -> false

(* leak/0x2120887e9e4889653770b228ee8d13231d0ef045_2 (transferAnything) *)
let eth_send_in_loop global (func : Func.t) = eth_send_in_loop' global func.body

let likely_privileged' : Global.t -> Func.t -> bool
= fun global func ->
  (* let b = eth_send_in_loop global func in *)
  (* let _ = print_endline (get_fname func ^ " : " ^ string_of_bool b) in *)
  exist_def_addr global func || eth_send_in_loop global func

(* The heuristic is based on the above contracts *)
let likely_accessible_by_all : Global.t -> Func.t -> bool
= fun global f ->
  let reachable = CallGraph.transitive_callees (BatSet.singleton (Func.fkey f)) global.call_edges in
  let reachable = BatSet.map (fun k -> FuncMap.find k global.fmap) reachable in
  let b1 = BatSet.exists (fun f' -> likely_by_all' global f') reachable in
  (* let b2 = BatSet.for_all (fun f' -> not (likely_privileged' global f')) reachable in
  let _ = print_endline (get_fname f ^ " : " ^ string_of_bool b1 ^ ", " ^ string_of_bool b2 ^ ", " ^ string_of_bool (contain_extern global f)) in *)
  b1 (* && b2 *)

(************************************)
(** Access-control Repair Template **)
(************************************)

(* utils for common repair templates *)
let collect_cnstr_cands ~def_var_num : Global.t -> Func.t list -> patch_comp list
= fun global funcs ->
  let call_edges = global.call_edges in
  (* constructors should be either 'public' or 'internal', *)
  (* but consider 'public' only because 'internal' cannot be directly invoked by users *)
  let funcs = List.filter Func.(fun f -> not (is_constructor f) && (is_public f)) funcs in
  List.fold_left (fun acc f ->
    let defs = FuncDefUse.find_d (Func.fkey f) global.f_defuse in
    let defs_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.global_vars)) defs in
    let addr_gvars = List.filter (fun v -> Typ.is_address (snd v)) global.global_vars |> BatSet.of_list |> BatSet.map fst in
    if not (BatSet.disjoint defs_gvars addr_gvars)
       && BatSet.cardinal defs >= def_var_num
       && not (BatSet.mem (Func.fkey f) (BatSet.map snd call_edges)) (* callee functions must not be candidates of constructors *)
       && f.info.org_scope_s = !Options.main_contract (* e.g., do not change setOwner funcitons in parents *)
      then (Atom (ChangeToCnstr (Func.fkey f))) :: acc
    else acc
  ) [] funcs

let is_range_address_of_mapping : Typ.t -> bool
= fun typ ->
  match typ with
  | Mapping (_, EType Address) -> true
  | Mapping _ -> false
  | _ -> false

let collect_funcs_that_define_addr_lvs : Global.t -> Func.t list -> Func.t list
= fun global funcs ->
  List.filter (fun f ->
    let defs = FuncDefUse.find_d (Func.fkey f) global.f_defuse in
    let defs_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.global_vars)) defs in
    let addr_gvars = List.filter (fun v -> Typ.is_address (snd v) || is_range_address_of_mapping (snd v)) global.global_vars |> BatSet.of_list |> BatSet.map fst in
    not (BatSet.disjoint defs_gvars addr_gvars)
  ) funcs

let collect_mods_that_use_addr_exps : Global.t -> Func.t list -> Func.t list
= fun global funcs ->
  let mods = List.filter Func.is_modifier funcs in
  let addr_gvars =
    let is_extended_address_kind (_,t) = (Typ.is_address_kind t || is_range_address_of_mapping t) in
    global.global_vars |> List.filter is_extended_address_kind |> BatSet.of_list |> BatSet.map fst
  in
  List.filter (fun f ->
    (* use 'use_set' rather than 'use_set_assume'; addr vars  may not be directly used in assume. *)
    let uses = FuncDefUse.find_u (Func.fkey f) global.f_defuse in
    let uses_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.global_vars)) uses in
    not (BatSet.disjoint uses_gvars addr_gvars)
  ) mods

(***************************)
(** Report-Aware Template **)
(***************************)

type line = int

let rec collect_loc : Stmt.t -> line BatSet.t
= fun stmt ->
  match stmt with
  | Assign (_,_,loc) -> BatSet.singleton loc.line
  | Decl _ -> BatSet.empty
  | Seq (s1,s2) -> BatSet.union (collect_loc s1) (collect_loc s2)
  | Call (_,_,_,_,_,loc) -> BatSet.singleton loc.line
  | Extcall e -> BatSet.singleton e.loc.line
  | Skip -> BatSet.empty
  | If (e,s1,s2,i) ->
    BatSet.add i.if_loc.line
      (BatSet.union (collect_loc_e e) (BatSet.union (collect_loc s1) (collect_loc s2)))
  | While (e,s) -> BatSet.union (collect_loc_e e) (collect_loc s)
  | Break | Continue -> BatSet.empty
  | Return (_,loc) -> BatSet.singleton loc.line
  | Revert -> BatSet.empty
  | Assume (_,loc) | Assert (_,_,loc) -> BatSet.singleton loc.line
  | Assembly _ | Placeholder | Label _ -> BatSet.empty
  | Unchecked (slst,loc) ->
    List.fold_left (fun acc s -> BatSet.union (collect_loc s) acc) (BatSet.singleton loc.line) slst

and collect_loc_e : exp -> line BatSet.t
= fun exp ->
  match exp with
  | V _ | Str _ -> BatSet.empty
  | Lv lv -> collect_loc_lv lv
  | Cast (_,e) -> collect_loc_e e
  | BinOp (_,_,_,einfo) -> BatSet.singleton einfo.loc.line
  | UnOp (_,e,_) -> collect_loc_e e
  | ETypeName _ -> BatSet.empty
  | _ -> assert false (* temp expressions *)

and collect_loc_lv : lv -> line BatSet.t
= fun lv ->
  match lv with
  | Var (_,vinfo)
  | MemberAccess (_,_,vinfo,_) -> BatSet.singleton vinfo.vloc.line
  | IndexAccess (e1,Some e2,_) -> BatSet.union (collect_loc_e e1) (collect_loc_e e2)
  | IndexAccess (_,None,_) -> assert false
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | None -> acc | Some e -> BatSet.union (collect_loc_e e) acc
    ) BatSet.empty eops

let get_toploc : Func.t -> line BatSet.t -> line option
= fun f mod_lines ->
  let lines = collect_loc f.body in
  let lines = BatSet.filter (fun l -> l > 0) lines in (* exclude modelled stmts *)
  let lines = BatSet.diff lines mod_lines in (* exclude lines from inliend modifiers *)
  if BatSet.is_empty lines then None
  else Some (BatSet.min_elt lines)

let add_authority_check_mod : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun global pgm f ->
  let mods = collect_mods_that_use_addr_exps global (Pgm.main pgm).funcs in
  let org_contract_of_f = Pgm.find_by_name pgm f.info.org_scope_s in
  let bases_of_f = List.map (Pgm.find_nid pgm) (org_contract_of_f.info).inherit_order |> List.map Contract.name in
  List.fold_left (fun acc m ->
    (* Do not apply modifiers defined in child contracts *)
    (* e.g., 0x9ac4883b0226fb37fa362dbd76564c539c4f0ee6_1: add 'onlyOwnerOrWhitelist' to '_transferOwnership' *)
    if not (List.mem m.Func.info.org_scope_s bases_of_f) then acc
    else if List.mem m.name (List.map (fun (m : Mod_call.t) -> m.id) f.info.mod_list) then acc

    else
      (Atom (AddModifier (Func.fkey f, m.name, f.info.param_loc.finish_line)))::acc
  ) [] mods

let add_authority_check_var : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun global pgm f ->
  let funcs = List.map snd (BatMap.bindings global.fmap) in
  let mods = List.filter Func.is_modifier funcs in
  let mod_body_lines = 
    List.fold_left (fun acc m -> BatSet.union (collect_loc m.Func.body) acc) BatSet.empty mods 
  in
  let mod_invoke_lines = List.map (fun (m : Mod_call.t) -> m.loc.line) f.info.mod_list |> BatSet.of_list in
  let mod_lines = BatSet.union mod_body_lines mod_invoke_lines in
  match get_toploc f mod_lines with (* TODO : refactoring *)
  | None -> []
  | Some loc ->
    let open State_var_decl in
    let decls = List.filter (fun d -> Typ.is_address d.vinfo.vtyp) ((Pgm.main pgm).decls) in
    List.fold_left (fun acc d ->
      let func_defined_cname = f.info.org_scope_s in
      let var_defined_cname = (List.find (fun c -> d.vinfo.vscope = Contract.numid c) pgm).name in
      (* avoid uncomilable patches *)
      if not (List.mem (d.name, d.vinfo.vtyp) (Contract.gvars (Pgm.find_by_name pgm func_defined_cname))) then acc (* TODO: add 'super' function for the if-case. *)
      else if not (!Options.main_contract = var_defined_cname) && d.vinfo.vvis = Private then acc
      else
        let e1 = Lv (Var (d.name, d.vinfo)) in
        let e2 = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
        let s = Stmt.Assume (mk_eq e1 e2, Loc.dummy) in
        let comp = Atom (InsertLine (loc, s, false)) in
        comp :: acc
    ) [] decls

(* protect with fresh ownership variable when no templates are applied. *)
let add_authority_check_var2 : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun global _pgm f ->
  let funcs = List.map snd (BatMap.bindings global.fmap) in
  let mods = List.filter Func.is_modifier funcs in
  let mod_body_lines = List.fold_left (fun acc m -> BatSet.union (collect_loc m.Func.body) acc) BatSet.empty mods in
  let mod_invoke_lines = List.map (fun (m : Mod_call.t) -> m.loc.line) f.info.mod_list |> BatSet.of_list in
  let mod_lines = BatSet.union mod_body_lines mod_invoke_lines in
  match get_toploc f mod_lines with
  | None -> assert false
  | Some loc ->
    let e1 = Lv (Var ("smartfix_owner",  mk_vinfo ~typ:(EType Address) ())) in
    let e2 = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
    let s = Stmt.Assume (mk_eq e1 e2, Loc.dummy) in
    let comp = Atom (InsertLine (loc, s, false)) in
    [comp]

(* template 2 *)
let negate_bools : exp list -> patch_comp list
= fun exps ->
  let is_address = Typ.is_address in
  List.fold_left (fun acc exp ->
    match exp with
    (* comparison with zero-address is not considered. *)
    | BinOp (bop,_,Cast (EType Address,V Int n),_) when (bop=Eq || bop=NEq) && Z.equal n Z.zero -> []
    | BinOp (bop,Cast (EType Address,V Int n),_,_) when (bop=Eq || bop=NEq) && Z.equal n Z.zero -> []

    | BinOp (Eq,e1,e2,einfo) when is_address (get_type_exp e1) && is_address (get_type_exp e2) ->
      acc @ [Atom (Replace (einfo.loc.line, exp, BinOp (NEq, e1, e2, {einfo with id = -1})))]
    | BinOp (NEq,e1,e2,einfo) when is_address (get_type_exp e1) && is_address (get_type_exp e2) ->
      acc @ [Atom (Replace (einfo.loc.line, exp, BinOp (Eq, e1, e2, {einfo with id = -1})))]
    | UnOp (LNot,Lv (Var (v,vinfo)),_) -> (* leak/0x9ac488..._1 *)
      acc @ [Atom (Replace (vinfo.vloc.line, exp, Lv (Var (v,vinfo))))] (* TODO: x => !x *)
    (* | UnOp (uop,e,typ) -> cand_e kind stmt_loc loc e *)
    | _ -> acc
  ) [] exps

(*****************************)
(** Report-Unaware Template **)
(*****************************)

(* common template 1. change to cnstr *)
let change_to_cnstr : Global.t -> Pgm.t -> Func.t list -> patch_comp list
= fun global _pgm funcs ->
  let cnstr = List.filter Func.is_constructor funcs |> (fun funcs' -> assert (List.length funcs' = 1); List.hd funcs') in
  let no_cnstr_in_org = cnstr.info.fid = -1 in
  if no_cnstr_in_org then
    let cands = collect_cnstr_cands ~def_var_num:2 global funcs in
    if List.length cands > 0 then cands
    else collect_cnstr_cands ~def_var_num:1 global funcs
  else []

(* common template 2. negate_cond_in_mod *)
let negate_cond_in_mod : Global.t -> Pgm.t -> Func.t list -> patch_comp list
= fun global _pgm funcs ->
  let mods = collect_mods_that_use_addr_exps global funcs in
  List.fold_left (fun acc m ->
    acc @ (negate_bools (List.map snd (collect_exp m.Func.body)))
  ) [] mods
  |> List.sort_uniq PatchComp.compare

let add_authority_check_to_func ~fresh_owner : Global.t -> Pgm.t -> Func.t -> patch_comp list -> patch_comp list
= fun global pgm f acc ->
  let mods_that_use_addrs = collect_mods_that_use_addr_exps global ((Pgm.main pgm).funcs) in
  let validation_with_vars = add_authority_check_var global pgm f in
  let no_global_addrs =
    let fields = Global.get_all_fields global in
    (* is_contract: reentrancy/0xfe1b613f17f984e27239b0b2dccfb1778888dfae *)
    let is_extended_address_kind t = (Typ.is_address_kind t || is_range_address_of_mapping t || Typ.is_contract t) in
    List.for_all (fun (_,t) -> not (is_extended_address_kind t)) (global.global_vars @ fields)
  in
  if Func.is_constructor f                     (* no input valiadation patches for constructors or modifers *)
     || likely_accessible_by_all global f (* heuristic : do not add ownership checking when likely to be normal functions *)
     || Func.is_private f || Func.is_internal f
    then acc
  else if List.length mods_that_use_addrs > 0 then acc @ (add_authority_check_mod global pgm f)
  else if List.length validation_with_vars > 0 then acc @ validation_with_vars
  else if no_global_addrs && fresh_owner then acc @ (add_authority_check_var2 global pgm f)
  else acc

(* common template 3. add_authority_check_to_addr *)
let add_authority_check_to_addr_def_funcs : Global.t -> Pgm.t -> Func.t list -> patch_comp list
= fun global pgm funcs ->
  let funcs = collect_funcs_that_define_addr_lvs global funcs in
  (* For natural patches: 0x2120887e9e4889653770b228ee8d13231d0ef045_3.sol *)
  let funcs = List.filter Func.(fun f -> is_public f || is_external f) funcs in
  (* let _ = print_endline "=========" in
  let _ = print_endline (Vocab.string_of_list get_fname funcs) in
  let _ = List.iter (fun f -> print_endline (get_fname f ^ " all accessible? : " ^ string_of_bool (likely_accessible_by_all global f))) funcs in
  let _ = List.iter (fun f -> print_endline (get_fname f ^ " privilaged?: " ^ string_of_bool (likely_privileged' global f))) funcs in
  let _ = assert false in  *)
  Vocab.list_fold (add_authority_check_to_func ~fresh_owner:false global pgm) funcs []
  |> List.sort_uniq PatchComp.compare

let report_unaware_template : Global.t -> Pgm.t -> patch_comp list
= fun global pgm ->
  let funcs = (Pgm.main pgm).funcs in
  let lst1 = change_to_cnstr global pgm funcs in
  let lst2 = negate_cond_in_mod global pgm funcs in
  let lst3 = add_authority_check_to_addr_def_funcs global pgm funcs in
  (lst1 @ lst2 @ lst3)
  |> List.sort_uniq PatchComp.compare

let report_aware_template : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun global pgm f ->
  let negated = 
    if likely_accessible_by_all global f then [] 
    else (negate_bools (List.map snd (collect_exp f.body))) in
  negated
  @ (add_authority_check_to_func ~fresh_owner:true global pgm f [])
  |> List.sort_uniq PatchComp.compare
