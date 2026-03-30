open! Frontend
open Frontend.Lang
open Vocab
open Vlang
open Semantics

type t = comps

and comps = {
  mapvars : Var.t BatSet.t;
  mapmems : (Var.t * Var.t) BatSet.t;
  reads   : ExpSet.t;
  ivars   : Var.t BatSet.t;
  avars   : Var.t BatSet.t;
  a_arrs  : Var.t BatSet.t;
  a_maps  : Var.t BatSet.t;
  bvars   : Var.t BatSet.t;
  ints    : Z.Set.t
}

let empty_comps = {
  mapvars = BatSet.empty;
  mapmems = BatSet.empty;
  reads = ExpSet.empty;
  ivars = BatSet.empty;
  avars = BatSet.empty;
  a_arrs = BatSet.empty;
  a_maps = BatSet.empty;
  bvars = BatSet.empty;
  ints = Z.Set.empty;
}

let subset : t -> t -> bool
= fun comps1 comps2 ->
  BatSet.subset comps1.mapvars comps2.mapvars
  && BatSet.subset comps1.mapmems comps2.mapmems
  && ExpSet.subset comps1.reads comps2.reads
  && BatSet.subset comps1.ivars comps2.ivars
  && BatSet.subset comps1.avars comps2.avars
  && BatSet.subset comps1.a_arrs comps2.a_arrs
  && BatSet.subset comps1.a_maps comps2.a_maps
  && BatSet.subset comps1.bvars comps2.bvars
  && Z.Set.subset comps1.ints comps2.ints

let union : t -> t -> t
= fun comps1 comps2 ->
  {mapvars = BatSet.union comps1.mapvars comps2.mapvars;
   mapmems = BatSet.union comps1.mapmems comps2.mapmems;
   reads = ExpSet.union comps1.reads comps2.reads;
   ivars = BatSet.union comps1.ivars comps2.ivars;
   avars = BatSet.union comps1.avars comps2.avars;
   a_arrs = BatSet.union comps1.a_arrs comps2.a_arrs;
   a_maps = BatSet.union comps1.a_maps comps2.a_maps;
   bvars = BatSet.union comps1.bvars comps2.bvars;
   ints = Z.Set.union comps1.ints comps2.ints
  }

let to_string : comps -> string
= fun comp ->
  let f f l = string_of_list ~first:"{" ~last:"}" ~sep:", " f l in
  "map vars     : " ^ f (fst|>id) (BatSet.to_list comp.mapvars) ^ "\n" ^
  "smap mems    : " ^ f (fun (x,y) -> fst x ^ "[i]." ^ fst y) (BatSet.to_list comp.mapmems) ^ "\n" ^
  "reads        : " ^ f to_string_vexp (ExpSet.to_list comp.reads) ^ "\n" ^
  "int vars     : " ^ f (fst|>id) (BatSet.to_list comp.ivars) ^ "\n" ^
  "addr vars    : " ^ f (fst|>id) (BatSet.to_list comp.avars) ^ "\n" ^
  "addr arrays  : " ^ f (fst|>id) (BatSet.to_list comp.a_arrs) ^ "\n" ^
  "addr maps    : " ^ f (fst|>id) (BatSet.to_list comp.a_maps) ^ "\n" ^
  "boolean vars : " ^ f (fst|>id) (BatSet.to_list comp.bvars) ^ "\n" ^
  "integers     : " ^ f Z.to_string (Z.Set.to_list comp.ints)

let is_addr_one_dim_array_typ : Typ.t -> bool
= fun typ ->
  match typ with
  | Array (t, _) when Typ.is_address_kind t -> true
  | _ -> false

let is_addr_kind_mapping_typ : Typ.t -> bool
= fun typ ->
  match typ with
  | Mapping (Address, t) when Typ.is_address_kind t -> true
  | _ -> false

let rec collect_e : Global.t -> exp -> t -> t
= fun global exp comps ->
  match exp with
  | V (Int n) -> {comps with ints = Z.Set.add n comps.ints}
  | Str _ -> comps
  | Lv lv ->
    if List.mem (to_string_lv lv) keyword_vars then comps
    else collect_lv global lv comps
  | Cast (EType Address, V (Int _)) -> comps
  | Cast (_,e) -> collect_e global e comps
  | BinOp (_,e1,e2,_) -> collect_e global e2 (collect_e global e1 comps)
  | UnOp (_,e,_) -> collect_e global e comps
  | V (Bool _) | ETypeName _ | IndexRangeAccess _ | TypeInfo _ -> comps
  | _ -> failwith "collect_e : temp expressions encountered"

and collect_lv (global : Global.t) (lv : lv) (comps : t) : t =
  match lv with
  | Var (x,_xinfo)
    when BatString.starts_with x "@"
         || BatString.starts_with x Translator.param_name || BatString.exists x (Inline.inline_mark)
    -> comps
  | Var (x,xinfo) ->
    let t = xinfo.vtyp in
    { comps with
      mapvars = if Typ.is_usual_mapping t then BatSet.add (x,t) comps.mapvars
                else comps.mapvars;
      ivars = if Typ.(is_uintkind t || is_sintkind t) then BatSet.add (x,t) comps.ivars
              else comps.ivars;
      avars = if Typ.is_address_kind t && not (Global.is_constant_address global (x,t)) then BatSet.add (x,t) comps.avars
              else comps.avars;
      a_arrs = if is_addr_one_dim_array_typ t then BatSet.add (x,t) comps.a_arrs
               else comps.a_arrs;
      a_maps = if is_addr_kind_mapping_typ t then BatSet.add (x,t) comps.a_maps
               else comps.a_maps;
      bvars = if Typ.is_bool t && List.mem (x,t) global.global_vars then BatSet.add (x,t) comps.bvars
              else comps.bvars;
    }

  | MemberAccess (e,x,xinfo,_)
    when Typ.is_struct (get_type_exp e) && Typ.is_uint256 xinfo.vtyp
         && not (BatSet.exists (fun y -> BatString.starts_with (fst y) "@") (var_exp e))
         && not (BatString.starts_with x "@")
    ->
    let pred = (fun g -> snd g = Typ.Mapping (Address, get_type_exp e)) in
    let lst = List.filter pred global.global_vars in
    List.fold_left (fun acc g ->
      {acc with mapmems = BatSet.add (g, (x, Typ.Mapping2 (get_type_exp e, xinfo.vtyp))) acc.mapmems}
    ) (collect_e global e comps) lst

  | MemberAccess (e,_x,_xinfo,_) -> collect_e global e comps
  | IndexAccess (e1,Some e2,_) ->
    let read = Read (convert_aexp e1, convert_aexp e2) in
    let vars = BatSet.union (var_exp e1) (var_exp e2) in
    (* e.g., exclude @Invest[...] *)
    let b = not (BatSet.exists (fun y -> BatString.starts_with (fst y) "@") vars) in
    let comps = {comps with reads = if b then ExpSet.add read comps.reads else comps.reads} in
    collect_e global e2 (collect_e global e1 comps)
  | IndexAccess (e,None,_) -> collect_e global e comps
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | None -> acc
      | Some e -> collect_e global e acc
    ) comps eops

let collect_s : Global.t -> Node.t -> Cfg.t -> t -> t
= fun global node g comps ->
  let stmt = Cfg.find_stmt node g in
  match stmt with
  | Assign (lv,e,_) -> collect_e global e (collect_lv global lv comps)
  | Decl lv -> collect_lv global lv comps
  | Call (lvop,e,elst,_,_,_) ->
    let comps = match lvop with None -> comps | Some lv -> collect_lv global lv comps in
    let comps = collect_e global e comps in
    List.fold_left (fun acc e' -> collect_e global e' acc) comps elst
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    ignore (id, fkey, ether, gas, is_static, loc);
    let comps = match ret with None -> comps | Some lv -> collect_lv global lv comps in
    let comps = collect_e global target comps in
    List.fold_left (fun acc e' -> collect_e global e' acc) comps args
  | Skip -> comps
  | Assume (e,_) -> collect_e global e comps
  | Assert (e,_,_) -> collect_e global e comps
  | Return (eop,_) -> (match eop with None -> comps | Some e -> collect_e global e comps)
  | Revert | Assembly _ | Label _ -> comps
  | Seq _ | If _ | While _
  | Break | Continue | Placeholder | Unchecked _ -> failwith ("collect_s: " ^ to_string_stmt stmt)

let collect_f : Global.t -> Func.t -> t
= fun global f ->
  let cfg = Func.cfg f in
  let nodes = MakeCfg.nodesof cfg in
  List.fold_left (fun acc node ->
    collect_s global node cfg acc
  ) empty_comps nodes

let collect_bp : Global.t -> Node.t list -> Cfg.t -> t
= fun global nodes cfg ->
  List.fold_left (fun acc node ->
    collect_s global node cfg acc
  ) empty_comps nodes
