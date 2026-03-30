open Patch
open Patch.PatchComp
open Frontend.Lang

let rec change_e : int -> exp -> patch_comp list
= fun line exp ->
  match exp with
  | V _ | Str _ -> []
  | Lv lv when to_string_lv lv = "tx.origin" ->
    let msg_sender = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
    let tx_origin = Lv (Var ("tx.origin", mk_vinfo ~typ:(EType Address) ())) in
    [Atom (Replace (line, tx_origin, msg_sender))]
  | Lv _ -> []
  | Cast (_,e) -> change_e line e
  | BinOp (_,e1,e2,_) -> (change_e line e1) @ (change_e line e2)
  | UnOp (_,e,_) -> change_e line e
  | ETypeName _ -> []
  | _ -> assert false

let change_s : Stmt.t -> patch_comp list
= fun stmt ->
  match stmt with
  | Assign (_lv,e,loc) -> change_e loc.line e
  | Assume (e,loc) -> change_e loc.line e
  | _ -> []

let generate : Global.t -> Pgm.t -> Func.t -> patch_comp list
= fun _global _pgm f ->
  let cfg = Func.cfg f in
  let nodes = MakeCfg.nodesof cfg in
  List.fold_left (fun acc n ->
    acc @ (change_s (Cfg.find_stmt n cfg))
  ) [] nodes
  |> List.sort_uniq PatchComp.compare
