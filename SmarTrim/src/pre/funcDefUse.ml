open Vocab
open! Frontend
open Frontend.Lang
open Frontend.FuncMap
open MakeCfg
open Vocab.Batteries

module V = struct
  type t = {
    d : string Set.t;  (** def *)
    u : string Set.t;  (** use *)
    a : string Set.t;  (** use in [assume] *)
  }
  [@@deriving fields ~getters ~fields]

  let empty =
    let e = Set.empty in
    { d = e; u = e; a = e }
  ;;
end

type t = (Fkey.t, V.t) Map.t

let mem x (m : t) = Map.mem x m
let empty : t = Map.empty
let bindings (m : t) = Map.bindings m
let find x (m : t) = try Map.find x m with Not_found -> V.empty (* for built-in functions *)
let find_d x (m : t) = find x m |> V.d
let find_u x (m : t) = find x m |> V.u
let find_a x (m : t) = find x m |> V.a

open struct
  let add_to_field field x s (m : t) =
    let v = find x m in
    Map.add x (Field.map field v ~f:(Set.union s)) m
  ;;
end

let add_def = add_to_field V.Fields.d
let add_use = add_to_field V.Fields.u
let add_use_assume = add_to_field V.Fields.a

let to_string : t -> string =
 fun map ->
  let to_string_y y = string_of_set ~first:"{" ~last:"}" ~sep:", " id y in
  "{" ^ "\n"
  ^ BatMap.foldi
      (fun x V.{ d; u; a } acc ->
        acc ^ Fkey.to_string x ^ " ->\n" ^ "  " ^ "DEF: " ^ to_string_y d ^ "\n" ^ "  " ^ "USE: "
        ^ to_string_y u ^ "\n" ^ "  " ^ "USE_ASSUME: " ^ to_string_y a ^ "\n")
      map ""
  ^ "}"
;;

(* XXX: It has a bug. Don't know the reason. *)
(* BatMap.equal BatSet.equal m1 m2 *)
let eq m1 m2 = BatString.equal (to_string m1) (to_string m2)

(* XXX: compute variables that may be defined in functions *)
(* XXX: should be changed in accordance with
 * XXX: 1) 'convert_lv' in semantics.ml
 * XXX: 2) 'get_target' in semantics.ml
 * XXX: 3) 'get_defs_lv' in funcDefVar.ml
 * XXX: 4) 'loc_of' in itvSem.ml *)
let rec get_def_set_lv : lv -> string BatSet.t =
 fun lv ->
  match lv with
  | Var (id, _vinfo) -> BatSet.singleton id
  | MemberAccess (_e, "length", _info, _) -> BatSet.singleton "@L"
  | MemberAccess (e, "balance", info, _) ->
    assert (info.vtyp = EType (UInt 256));
    assert (Typ.is_address_kind (get_type_exp e));
    BatSet.singleton "@B"
  | MemberAccess (_, x, _, _) -> BatSet.singleton x
  (* | MemberAccess (Lv lv,x,xinfo,_) -> get_def_set_lv lv *)
  | IndexAccess (Lv lv, _, _) -> get_def_set_lv lv
  | IndexAccess (Cast (_, Lv lv), _, _) -> get_def_set_lv lv
  | Tuple (eops, _) ->
    List.fold_left
      (fun acc eop ->
        match eop with
        | Some (Lv lv) -> BatSet.union (get_def_set_lv lv) acc
        | None -> acc
        | _ -> failwith "get_defs_lv")
      BatSet.empty eops
  | _ -> failwith "get_defs_lv"
;;

let get_def_set_lvop lvop = match lvop with None -> BatSet.empty | Some lv -> get_def_set_lv lv
let get_use_set_exp exp = BatSet.map fst (var_exp exp)

let get_use_set_exps exps =
  List.fold_left
    (fun acc e ->
      let uses = get_use_set_exp e in
      BatSet.union uses acc)
    BatSet.empty exps
;;

type alias_map = (string, string BatSet.t) BatMap.t

let find_alias x m = try BatMap.find x m with Not_found -> BatSet.empty

let handle_built_in_funcs (lvop, f, args) =
  match lvop with
  | None -> begin
    match f with
    | "revert" -> (BatSet.empty, BatSet.empty)
    | "selfdestruct" -> (BatSet.empty, get_use_set_exps args)
    | "suicide" -> (BatSet.empty, get_use_set_exps args)
    | "delete" ->
      let _ = assert (List.length args = 1) in
      let exp = List.hd args in
      let defs = match exp with Lv lv -> get_def_set_lv lv | _ -> assert false in
      (defs, BatSet.diff (get_use_set_exp exp) defs)
    | _ -> failwith ("VeriSmart.Pre.FuncDefUse.handle_built_in_funcs (None-branch): " ^ f)
  end
  | Some lv ->
    (* lv is in 'Var' pattern *)
    begin
      match f with
      | "keccak256" | "sha3" | "sha256" | "ripemd160" | "ecrecover" | "addmod" | "mulmod"
      | "blockhash" | "gasleft" ->
        (get_def_set_lv lv, get_use_set_exps args)
      | _ -> failwith ("VeriSmart.Pre.FuncDefUse.handle_built_in_funcs (Some-branch): " ^ f)
    end
;;

let handle_undefs (_fmap : FuncMap.t) (lvop, exp, args) _loc =
  match exp with
  | Lv (Var (f, _)) when List.mem f built_in_funcs -> handle_built_in_funcs (lvop, f, args)
  | _ -> (get_def_set_lvop lvop, get_use_set_exps args)
;;

(* TODO *)

let update_du_n :
    string list -> FuncMap.t -> Cfg.t -> Fkey.t -> Node.t -> t * alias_map -> t * alias_map =
 fun cnames fmap g fkey n (du_map, alias_map) ->
  let cname = g.signature.contract in
  let stmt = find_stmt n g in
  match stmt with
  | Assign (lv, Lv e, _) when Typ.is_struct (get_type_lv lv) ->
    let defs, aliases = (get_def_set_lv lv, get_def_set_lv e) in
    let uses =
      let lv_use = BatSet.diff (BatSet.map fst (var_lv lv)) (get_def_set_lv lv) in
      BatSet.union lv_use (BatSet.map fst (var_exp (Lv e)))
    in
    let du_map = add_def fkey defs du_map in
    let du_map = add_use fkey uses du_map in
    let alias_map =
      BatSet.fold
        (fun d acc -> BatMap.add d (BatSet.union aliases (find_alias d acc)) acc)
        defs alias_map
    in
    (du_map, alias_map)
  | Assign (lv, _, _) when Typ.is_struct (get_type_lv lv) -> assert false
  | Assign (lv, e, _) ->
    let defs = get_def_set_lv lv in
    let defs = BatSet.fold (fun d acc -> BatSet.union (find_alias d alias_map) acc) defs defs in
    let uses =
      let lv_use = BatSet.diff (BatSet.map fst (var_lv lv)) (get_def_set_lv lv) in
      BatSet.union lv_use (BatSet.map fst (var_exp e))
    in
    let du_map = add_def fkey defs du_map in
    let du_map = add_use fkey uses du_map in
    (du_map, alias_map)
  | Decl lv -> (add_def fkey (get_def_set_lv lv) du_map, alias_map)
  | Call (lvop, e, args, _, _, loc) when is_undef_call fmap stmt ->
    let defs, uses = handle_undefs fmap (lvop, e, args) loc in
    let du_map = add_def fkey defs du_map in
    let du_map = add_use fkey uses du_map in
    (du_map, alias_map)
  | Extcall _ ->
    (* the anlysis here aims to identify def-use by direct usage in code *)
    (du_map, alias_map)
  | Call (lvop, e, args, _, _, _) ->
    (* normal calls *)
    (* Find def set *)
    let init_defs = match lvop with Some lv -> get_def_set_lv lv | None -> BatSet.empty in
    let callees = FuncMap.find_matching_funcs cname e (List.map get_type_exp args) cnames fmap in
    let defs =
      BatSet.fold
        (fun callee acc -> BatSet.union (find_d (Func.fkey callee) du_map) acc)
        callees init_defs
    in
    (* Find use set *)
    let init_uses1 =
      match lvop with
      | Some lv ->
        let all = BatSet.map fst (var_lv lv) in
        BatSet.diff all (get_def_set_lv lv)
      | None -> BatSet.empty
    in
    let init_uses2 =
      List.fold_left
        (fun acc e ->
          let use_ids = BatSet.map fst (var_exp e) in
          BatSet.union use_ids acc)
        BatSet.empty args
    in
    let init_uses = BatSet.union init_uses1 init_uses2 in
    let uses =
      BatSet.fold
        (fun callee acc -> BatSet.union (find_u (Func.fkey callee) du_map) acc)
        callees init_uses
    in
    (* Find use_assume set *)
    let uses_assume =
      BatSet.fold
        (fun callee acc -> BatSet.union (find_a (Func.fkey callee) du_map) acc)
        callees BatSet.empty
    in
    (du_map |> add_def fkey defs |> add_use fkey uses |> add_use_assume fkey uses_assume, alias_map)
  | Assembly (lst, _) ->
    let defs = BatSet.map fst (BatSet.of_list lst) in
    (* conservative result *)
    let uses = defs in
    (du_map |> add_def fkey defs |> add_use fkey uses, alias_map)
  | Skip -> (du_map, alias_map)
  | Return (None, _) -> (du_map, alias_map)
  | Return (Some e, _) ->
    let uses = BatSet.map fst (var_exp e) in
    (add_use fkey uses du_map, alias_map)
  | Revert -> (du_map, alias_map)
  | Assume (e, _) ->
    let uses = BatSet.map fst (var_exp e) in
    (du_map |> add_use fkey uses |> add_use_assume fkey uses, alias_map)
  | Assert (e, _, _) ->
    let uses = BatSet.map fst (var_exp e) in
    (add_use fkey uses du_map, alias_map)
  | Placeholder | Label _ ->
    (du_map, alias_map) (* this case is encountered for the modifier case *)
  | If _ | Seq _ | While _ | Break | Continue | Unchecked _ -> failwith "update_du_n"
;;

let onestep : string list -> FuncMap.t -> Fkey.t -> t * alias_map -> t * alias_map =
 fun cnames fmap fkey (du_map, alias_map) ->
  let f = FuncMap.find fkey fmap in
  let g = Func.cfg f in
  let nodes = Cfg.nodes_of g in
  List.fold_left
    (fun (acc_du_map, acc_alias_map) n ->
      update_du_n cnames fmap g fkey n (acc_du_map, acc_alias_map))
    (du_map, alias_map) nodes
;;

let onestep_f : string list -> FuncMap.t -> Fkey.t BatSet.t -> t * alias_map -> t * alias_map =
 fun cnames fmap fkeys (du_map, alias_map) ->
  BatSet.fold
    (fun fkey (acc_du_map, acc_alias_map) -> onestep cnames fmap fkey (acc_du_map, acc_alias_map))
    fkeys (du_map, alias_map)
;;

let rec fix_fsum : string list -> FuncMap.t -> Fkey.t BatSet.t -> t * alias_map -> t =
 fun cnames fmap fkeys (du_map, alias_map) ->
  let du_map', alias_map' = onestep_f cnames fmap fkeys (du_map, alias_map) in
  if eq du_map' du_map then du_map' else fix_fsum cnames fmap fkeys (du_map', alias_map')
;;

let compute : string list -> FuncMap.t -> t =
 fun cnames fmap ->
  let lst = BatMap.bindings fmap in
  (* let lst = List.filter (fun (fkey,f) -> not (is_modifier f)) lst in *)
  (* modifiers' information is used in patchACC module *)
  let fkeys = List.map fst lst |> BatSet.of_list in
  let res = fix_fsum cnames fmap fkeys (empty, BatMap.empty) in
  res
;;
