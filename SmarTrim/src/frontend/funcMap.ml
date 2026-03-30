open Lang

exception FunctionNotFound of string * string

type t = (Fkey.t, Func.t) Map.t

let pp = Map.pp ~kvsep:(Pp.lit ":") Fkey.pp (Pp.using Func.name Pp.string)
let empty = Map.empty
let add = Map.add
let mem = Map.mem
let fold = Map.foldi
let merge m1 m2 = Map.foldi add m1 m2
let dom m = Set.of_enum (Map.keys m)

let find2 (fid, arg_typs) (map : t) =
  Map.foldi
    (fun Fkey.{ func = f; param_typs; _ } v acc ->
      if
        String.equal fid f
        && List.length arg_typs = List.length param_typs
        && List.for_all2 Typ.is_implicitly_convertible arg_typs param_typs
      then Set.add v acc
      else acc)
    map Set.empty
;;

let find Fkey.{ contract = c; func = f; param_typs } (map : t) =
  let funcs =
    Map.foldi
      (fun Fkey.{ contract = c'; func = f'; param_typs = p' } v acc ->
        if
          String.equal c c' && String.equal f f'
          && List.length param_typs = List.length p'
          && List.for_all2 Typ.is_implicitly_convertible param_typs p'
        then Set.add v acc
        else acc)
      map Set.empty
  in
  if Set.cardinal funcs = 1 then Set.choose funcs
  else if Set.cardinal funcs = 0 then raise (FunctionNotFound (c, f))
  else failwith "FuncMap.find"
;;

let find_opt key map = try Some (find key map) with FunctionNotFound _ -> None

let to_string (map : t) : string =
  let to_string_y y = string_of_int y.Func.info.fid in
  "{" ^ "\n"
  ^ Map.foldi (fun x y acc -> acc ^ Fkey.to_string x ^ " -> " ^ to_string_y y ^ "\n") map ""
  ^ "}"
;;

let mk_fmap_c (c : Contract.t) (fmap : t) : t =
  List.fold_left
    (fun acc f ->
      let fid, typs = Func.fsig f in
      add Fkey.{ contract = c.name; func = fid; param_typs = typs } f acc)
    fmap c.funcs
;;

let mk_fmap_p (contracts : Pgm.t) : t =
  List.fold_left (fun acc contract -> mk_fmap_c contract acc) empty contracts
;;

let mk_fmap = mk_fmap_p

let is_undef exp arg_typs (fmap : t) =
  match exp with
  | Lv (MemberAccess (e, _fname, _, _)) ->
    (* let fs = find2 (fname, arg_typs) fmap in *)
    if Typ.is_contract (get_type_exp e) then
      (* 'fs' can be the empty set, e.g., getter function for mappings *)
      (* let _ = assert (not (BatSet.is_empty fs)) in *)
      false
    else
      (* let _ = assert (BatSet.is_empty fs) in *)
      (* e.g., abi.encode *)
      true
  | Lv (Var (fname, _)) ->
    let fs = find2 (fname, arg_typs) fmap in
    Set.is_empty fs
  | _ -> Pp.failwithf "is_undef : %s" (to_string_exp exp)
;;

let is_undef_call (fmap : t) (stmt : Stmt.t) : bool =
  match stmt with
  | Call (_, e, args, _, _, _) when is_undef e (List.map get_type_exp args) fmap -> true
  | _ -> false
;;

let is_internal_call (fmap : t) (cnames : string list) (stmt : Stmt.t) : bool =
  match stmt with
  | Call _ when is_undef_call fmap stmt -> false
  | Call (_, Lv (Var (_, _)), _, _, _, _) -> true
  | Call (_, Lv (MemberAccess (Lv (Var (x, _)), _, _, _)), _, _, _, _) when List.mem x cnames ->
    true
  | _ -> false
;;

let is_internal_call_node (fmap : t) (cnames : string list) (n : Node.t) (g : Cfg.t) : bool =
  is_internal_call fmap cnames (Cfg.find_stmt n g)
;;

let is_external_call_node (n : Node.t) (g : Cfg.t) : bool =
  match Cfg.find_stmt n g with Extcall _ -> true | _ -> false
;;

let find_matching_funcs (cname : string) (e : exp) (arg_typs : Typ.t list) (cnames : string list)
    (fmap : t) : Func.t Set.t =
  match e with
  | Lv (Var (fname, _)) ->
    (* TODO: remove *)
    Set.singleton (find (Fkey.mk cname fname arg_typs) fmap)
  | Lv (MemberAccess (Lv (Var (prefix, _)), fname, _, _)) ->
    if List.mem prefix cnames then (* static call *)
      Set.singleton (find (Fkey.mk prefix fname arg_typs) fmap)
    else (* method call *)
      find2 (fname, arg_typs) fmap
  | Lv (MemberAccess (_, fname, _, _)) -> find2 (fname, arg_typs) fmap
  | _ -> failwith ("find_matching_funcs : " ^ to_string_exp e)
;;

let find_modifier_by_name (fmap : t) name =
  ( Map.bindings fmap
  |> List.filter (fun ((fkey : Fkey.t), _) ->
         !Options.main_contract = fkey.contract && fkey.func = name)
  |> fun lst ->
    assert (List.length lst = 1);
    snd (List.hd lst) )
  |> fun res ->
  assert (Func.is_modifier res);
  res
;;
