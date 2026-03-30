open! Frontend
open Frontend.Lang
open Vocab

let stopwords = ["_"; "Of"]

let print_map map =
  print_endline "{";
  BatMap.iter (fun x y ->
    print_endline (fst x ^ " -> " ^ fst y)
  ) map;
  print_endline "}"

let remove_stopwords x =
  List.fold_left (fun acc sw ->
    BatString.nreplace ~str:acc ~sub:sw ~by:""
  ) x stopwords

let preprocess x = x |> Var.origin_name |> remove_stopwords

let is_oov : Py.Object.t -> string -> bool
= fun wv x ->
  let x = Py.String.of_string (preprocess x) in
  let gensim = Py.import "src.bugsynth.py.mygensim" in
  Py.Module.get_function gensim "contain" [|wv; x|]
  |> Py.Bool.to_bool
  |> (fun res -> not res)

let similarity : Py.Object.t -> string -> string -> float
= fun wv x y ->
  let x, y = Py.String.of_string (preprocess x), Py.String.of_string (preprocess y) in
  let gensim = Py.import "src.bugsynth.py.mygensim" in
  Py.Module.get_function gensim "similarity" [|wv; x;  y|]
  |> Py.Float.to_float

let is_similar_var wv x y =
  (* usefulness of 1st condition *)
  (* E.g., sim('balances', 'account') > sim('to', 'account') *)
  if preprocess (fst x) = preprocess (fst y) then snd x = snd y
  else if is_oov wv (fst x) || is_oov wv (fst y) then false
  else
    (snd x) = (snd y) && similarity wv (fst x) (fst y) >= 0.3

let exist_match : Py.Object.t -> Var.t -> Var.t BatSet.t -> bool
= fun wv x ys ->
  BatSet.exists (is_similar_var wv x) ys
  (* let res = similarity wv x y in
  let _ = print_endline (fst x ^ ", "  ^ fst y ^ ", " ^ string_of_float res) in *)

let matched_for_all : Py.Object.t -> Var.t BatSet.t -> Var.t BatSet.t -> bool
= fun wv xs ys ->
  BatSet.for_all (fun x -> exist_match wv x ys) xs

let find_best_match : Py.Object.t -> Var.t -> Var.t BatSet.t -> float * Var.t option
= fun wv x ys ->
  BatSet.fold (fun y (max,max_y) ->
    if preprocess (fst x) = preprocess (fst y) then  (* e.g., msg.sender *)
      (1.0, Some y)
    else if is_oov wv (fst x) || is_oov wv (fst y) then
      (max, max_y)
    else
      let res = similarity wv (fst x) (fst y) in
      if res > max && is_similar_var wv x y then (res, Some y)
      else (max, max_y)
  ) ys (0.0, None)

exception VarMatch_Failed

let mk_map : Py.Object.t -> Var.t BatSet.t -> Var.t BatSet.t -> (bool * (Var.t, Var.t) BatMap.t)
= fun wv xs ys ->
  try
    BatSet.fold (fun x (b,acc) ->
      match find_best_match wv x ys with
      | (_, None) -> raise VarMatch_Failed
      | (_, Some y) -> (b, BatMap.add x y acc)
    ) xs (true, BatMap.empty)
  with VarMatch_Failed -> (false, BatMap.empty)

let mk_map2 : Py.Object.t ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (bool * (Var.t, Var.t) BatMap.t)
= fun wv (xs1,ys1) (xs2,ys2) ->
  let (success1,map1) = mk_map wv xs1 ys1 in
  let (success2,map2) = mk_map wv xs2 ys2 in
  (success1 && success2, BatMap.union map1 map2)

let mk_map3 : Py.Object.t ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (bool * (Var.t, Var.t) BatMap.t)
= fun wv (xs1,ys1) (xs2,ys2) (xs3,ys3) ->
  let (b1,map1) = mk_map wv xs1 ys1 in
  let (b2,map2) = mk_map wv xs2 ys2 in
  let (b3,map3) = mk_map wv xs3 ys3 in
  (b1 && b2 && b3,
   map1 |> BatMap.union map2 |> BatMap.union map3)

let rec replace_lv : (Var.t, Var.t) BatMap.t -> lv -> lv
= fun map lv ->
  match lv with
  | Var (_,xinfo) when Typ.is_void xinfo.vtyp -> lv
  | Var (_x,xinfo) when Typ.is_func xinfo.vtyp -> lv (* TODO *)
  | Var (x,xinfo) ->
    (* let _ = print_endline (x ^ " : " ^ to_string_typ xinfo.vtyp) in *)
    let y = BatMap.find (x,xinfo.vtyp) map in
    let yinfo = {xinfo with org = Some (Lv (Var (Var.origin_name (fst y), mk_vinfo ~typ:(snd y) ())))} in
    Var (fst y, yinfo)
  | MemberAccess (_,_,_,typ) when Typ.is_enum typ -> lv
  | MemberAccess (e,x,xinfo,typ) -> MemberAccess (replace_e map e, x, xinfo, typ)
  | IndexAccess (_,None,_) -> failwith "replace_lv"
  | IndexAccess (e1,Some e2,typ) -> IndexAccess (replace_e map e1, Some (replace_e map e2), typ)
  | Tuple (eoplst,typ) ->
    List.map (fun eop ->
      match eop with
      | None -> None | Some e -> Some (replace_e map e)
    ) eoplst
    |> (fun eoplst' -> Tuple (eoplst',typ))

and replace_e : (Var.t, Var.t) BatMap.t -> exp -> exp
= fun map exp ->
  match exp with
  | V _ | Str _ -> exp
  | Lv lv ->
    if List.mem (to_string_lv lv) keyword_vars || to_string_lv lv = "abi" then exp
    else Lv (replace_lv map lv)
  | Cast (typ,e) -> Cast (typ, replace_e map e)
  | BinOp (bop,e1,e2,einfo) ->
    BinOp (bop, replace_e map e1, replace_e map e2, einfo)
  | UnOp (uop,e,typ) -> UnOp (uop, replace_e map e, typ)
  | Ite (i, t, e, typ) -> Ite (replace_e map i, replace_e map t, replace_e map e, typ)
  | ETypeName _ -> exp
  | IndexRangeAccess (base,sop,fop,einfo) -> IndexRangeAccess (replace_lv map base, replace_eop map sop, replace_eop map fop, einfo)
  | TypeInfo _ -> raise NotImplemented
  | IncTemp _ | DecTemp _ -> raise NotImplemented
  | CallTemp (e,args,ethop,gasop,einfo) ->
    let e' = replace_e map e in (* balances[account] => balances[to] *)
    let args' = List.map (replace_e map) args in
    let ethop' = replace_eop map ethop in
    let gasop' = replace_eop map gasop in
    CallTemp (e',args',ethop',gasop',einfo)
  | CondTemp _ | AssignTemp _ -> raise NotImplemented

and replace_eop : (Var.t, Var.t) BatMap.t -> exp option -> exp option
= fun map eop ->
  match eop with
  | Some e -> Some (replace_e map e)
  | None -> None

let rec replace_s : (Var.t, Var.t) BatMap.t -> Stmt.t -> Stmt.t
= fun map stmt ->
  match stmt with
  | Assign (lv,e,loc) -> Assign (replace_lv map lv, replace_e map e, loc)
  | Decl lv -> Decl (replace_lv map lv)
  | Call (lvop,e,args,ethop,gasop,loc) ->
    let lvop' = match lvop with None -> lvop | Some lv -> Some (replace_lv map lv) in
    let e' = replace_e map e in
    let args' = List.map (replace_e map) args in
    let ethop' = match ethop with None -> ethop | Some e -> Some (replace_e map e) in
    let gasop' = match gasop with None -> gasop | Some e -> Some (replace_e map e) in
    Call (lvop',e',args',ethop',gasop',loc)
  | Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc } ->
    let ret = match ret with None -> ret | Some lv -> Some (replace_lv map lv) in
    let target = replace_e map target in
    let args = List.map (replace_e map) args in
    let ether = match ether with None -> ether | Some e -> Some (replace_e map e) in
    let gas = match gas with None -> gas | Some e -> Some (replace_e map e) in
    Extcall { id; ret; fkey; target; args; ether; gas; is_static; loc }
  | Skip -> stmt
  | Return (None,_) -> stmt
  | Return (Some e,loc) -> Return (Some (replace_e map e), loc)
  | Revert -> stmt
  | Assume (e,loc) -> Assume (replace_e map e, loc)
  | Assert (e,vtyp,loc) -> Assert (replace_e map e, vtyp, loc)
  | Assembly (_lst,_loc) -> stmt (* TODO *)
  | If (e,s1,s2,i) -> If (replace_e map e, replace_s map s1, replace_s map s2, i)
  | Seq (s1,s2) -> Seq (replace_s map s1, replace_s map s2)
  | While (e,s) -> While (replace_e map e, replace_s map s)
  | Break | Continue | Placeholder | Label _ -> stmt
  | Unchecked (lst,loc) ->
    let lst' = List.map (replace_s map) lst in
    Unchecked (lst', loc)
