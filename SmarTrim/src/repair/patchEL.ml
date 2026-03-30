open Patch
open Patch.PatchComp
open! Frontend
open Frontend.Lang
open GenPatchUtil

(***********************************)
(** Ether leak Repair Template    **)
(***********************************)

type line = int

let rec collect_eth_transfer : Global.t -> line -> Stmt.t -> (lv * Stmt.t) list
= fun global l stmt ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (collect_eth_transfer global l s1) @ (collect_eth_transfer global l s2)
  | Call (_lvop,Lv (MemberAccess (e,fname,_,_)),args,_,_,loc)
    when Typ.is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] && l = loc.line ->
    let eth = (assert (List.length args = 1); List.hd args) in
    (match eth with
     | Lv (Var _) -> []
     | Lv lv -> if BatSet.disjoint (var_lv lv) (BatSet.of_list global.global_vars) then [] else [(lv, stmt)]
     | _ -> [])
  | Call (_lvop, Lv (MemberAccess (e,"call",_,_)), _args, Some eth, _gasop, loc)
    when Typ.is_address_kind (get_type_exp e) && l = loc.line ->
    (match eth with
     | Lv (Var _) -> []
     | Lv lv -> if BatSet.disjoint (var_exp eth) (BatSet.of_list global.global_vars) then [] else [(lv, stmt)]
     | _ -> [])
  | Call _ -> []
  | Extcall _ -> []
  | Skip -> []
  | If (_,s1,s2,_) -> (collect_eth_transfer global l s1) @ (collect_eth_transfer global l s2)
  | While (_,s) -> collect_eth_transfer global l s
  | Break | Continue | Return _
  | Revert | Assume _ | Assert _
  | Assembly _ | Placeholder | Label _ -> []
  | Unchecked (slst,_) -> List.fold_left (fun acc s -> acc @ (collect_eth_transfer global l s)) [] slst

let leak_fix : Global.t -> Func.t -> line -> patch_comp list
= fun global f line ->
  let assigns = get_assigns line f.body in
  let eth_transfer = collect_eth_transfer global line f.body in
  if List.length eth_transfer = 0 then []
  else
    let (eth_lv,_) = (assert (List.length eth_transfer = 1); List.hd eth_transfer) in
    let assigns = List.filter (fun (lv',_,_) -> to_string_lv lv' = to_string_lv eth_lv) assigns in
    if List.length assigns > 0 then []
    else (* x.transfer(b[m]) => uint tmp = b[m]; b[m] := 0 ; x.transfer(tmp); *)
      let tmp = fresh_tmp () in
      let tmp = Var (tmp, mk_vinfo ~typ:(get_type_lv eth_lv) ()) in
      let a1 = Replace (line, Lv eth_lv, Lv tmp) in
      let a2 = InsertLine (line, Assign (eth_lv, V (Int Z.zero), Loc.dummy), false) in
      let a3 = InsertLine (line, Assign (tmp, Lv eth_lv, Loc.dummy), false) in
      [AtomLst [a1;a2;a3]]

let generate : Global.t -> Pgm.t -> Func.t -> line -> patch_comp list
= fun global pgm f line ->
  (leak_fix global f line)
  @ (PatchACC.report_aware_template global pgm f)
