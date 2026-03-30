open! Frontend
open Frontend.Lang
open Vocab

let nodesof : Cfg.t -> Node.t list
= fun g -> G.fold_vertex (fun x l -> x :: l) g.graph []

(* unconditionally add edges *)
let add_edge : Node.t -> Node.t -> Cfg.t -> Cfg.t
= fun n1 n2 g -> {g with graph = G.add_edge g.graph n1 n2}

let remove_edge : Node.t -> Node.t -> Cfg.t -> Cfg.t
= fun n1 n2 g -> {g with graph = G.remove_edge g.graph n1 n2}

let add_node : Node.t -> Cfg.t -> Cfg.t
= fun n g -> {g with graph = G.add_vertex g.graph n}

let remove_node : Node.t -> Cfg.t -> Cfg.t
= fun n g ->
  { g with graph = G.remove_vertex g.graph n;
           stmt_map = BatMap.remove n g.stmt_map;
           outpreds_of_lh = BatSet.remove n g.outpreds_of_lh;
           lh_set = BatSet.remove n g.lh_set;
           lx_set = BatSet.remove n g.lx_set;
           continue_set = BatSet.remove n g.continue_set;
           break_set = BatSet.remove n g.break_set;
  }

let fold_node f (g : Cfg.t) acc = G.fold_vertex f g.graph acc
let fold_edges f (g : Cfg.t) acc = G.fold_edges f g.graph acc

let find_stmt : Node.t -> Cfg.t -> Stmt.t
= fun n g -> 
  try if n = Node.ENTRY || n = Node.EXIT then Skip
      else BatMap.find n g.stmt_map
  with _ -> raise (Failure ("No stmt found in the given node " ^ Node.to_string n))

let add_stmt : Node.t -> Stmt.t -> Cfg.t -> Cfg.t
= fun n s g -> {g with stmt_map = BatMap.add n s g.stmt_map}

let add_node_stmt : Node.t -> Stmt.t -> Cfg.t -> Cfg.t
= fun n s g -> g |> add_node n |> add_stmt n s

let pred : Node.t -> Cfg.t -> Node.t list
= fun n g -> G.pred g.graph n

let succ : Node.t -> Cfg.t -> Node.t list
= fun n g -> G.succ g.graph n

let rec has_break_cont (s : Stmt.t) : bool =
  match s with
  | Assign _ | Decl _ | Call _ | Extcall _ | Skip -> false
  | Seq (s1,s2) -> has_break_cont s1 || has_break_cont s2
  | If (_,s1,s2,_) -> has_break_cont s1 || has_break_cont s2
  | While _ -> false (* must consider outer loop only. *)
  | Break | Continue -> true
  | Return _ | Revert | Assume _ | Assert _ | Assembly _ -> false
  | Placeholder | Unchecked (_,_) -> assert false
  | Label _ -> false

let rec trans : Stmt.t -> Node.t option -> Node.t option -> (Node.t * Cfg.t) -> (Node.t * Cfg.t)
= fun stmt lhop lxop (n,g) -> (* lhop : header of dominant loop, lxop : exit of dominant loop, n: interface node *)
  match stmt with
  | Seq (s,While (e,s')) when has_break_cont s ->
    (* do-while with 'Break' or 'Continue' in loop-body *)
    let lh = Node.make () in
    let lx = Node.make () in
    let (n1,g1) = trans s (Some lh) (Some lx) (n,g) in
    let g2 = g1 |> add_node_stmt lh Skip |> add_edge n1 lh in
    let (n3,g3) = trans (Assume (e, Loc.dummy)) (Some lh) (Some lx) (lh,g2) in
    let (n4,g4) = trans s' (Some lh) (Some lx) (n3,g3) in
    let g5 = add_edge n4 lh g4 in
    let (n6,g6) = trans (Assume (UnOp (LNot,e,EType Bool), Loc.dummy)) lhop lxop (lh,g5) in
    let g7 = g6 |> add_node_stmt lx Skip |> add_edge n6 lx in
    let preds_of_lh = BatSet.of_list (pred lh g2) in
    let _ = assert (BatSet.mem n1 preds_of_lh) in
    let g8 = {g7 with outpreds_of_lh = BatSet.union preds_of_lh g7.outpreds_of_lh; lh_set = BatSet.add lh g7.lh_set; lx_set = BatSet.add lx g7.lx_set} in
    (lx, g8)
  | Seq (s1,s2) -> trans s2 lhop lxop (trans s1 lhop lxop (n,g))
  | If (e,s1,s2,_) ->
    let loc = match e with BinOp (_,_,_,einfo) -> einfo.loc | _ -> Loc.dummy in
    let (tn,g1) = trans (Assume (e, loc)) lhop lxop (n,g) in (* tn: true branch assume node *)
    let (tbn,g2) = trans s1 lhop lxop (tn,g1) in
    let (fn,g3) = trans (Assume (UnOp (LNot,e,EType Bool), loc)) lhop lxop (n,g2) in (* fn: false branch assume node *)
    let (fbn,g4) = trans s2 lhop lxop (fn,g3) in
    let join = Node.make () in
    let g5 = g4 |> add_node_stmt join Skip |> add_edge tbn join |> add_edge fbn join in
    (join, g5)
  | While (e,s) ->
    let lh = Node.make () in
    let lx = Node.make () in (* node id : lh + 1 *)
    let g1 = g |> add_node_stmt lh Skip |> add_edge n lh in
    let (n2,g2) = trans (Assume (e, Loc.dummy)) (Some lh) (Some lx) (lh,g1) in (* node id : lh + 2 *)
    let (n3,g3) = trans s (Some lh) (Some lx) (n2,g2) in
    let g4 = add_edge n3 lh g3 in
    let (n5,g5) = trans (Assume (UnOp (LNot,e,EType Bool), Loc.dummy)) lhop lxop (lh,g4) in
    let g6 = g5 |> add_node_stmt lx Skip |> add_edge n5 lx in
    let g7 = {g6 with outpreds_of_lh = BatSet.add n g6.outpreds_of_lh; lh_set = BatSet.add lh g6.lh_set; lx_set = BatSet.add lx g6.lx_set} in
    (lx, g7)
  | Break ->
    let lx = (match lxop with Some lx -> lx | None -> raise (Failure "Loop exit should exist")) in
    let n' = Node.make () in
    let g' = g |> add_node_stmt n' Skip |> add_edge n n' |> add_edge n' lx in
    (n', {g' with break_set = BatSet.add n' g'.break_set})
  | Continue ->
    let lh = (match lhop with Some lh -> lh | None -> raise (Failure "Loop header should exist")) in
    let n' = Node.make () in
    let g' = g |> add_node_stmt n' Skip |> add_edge n n' |> add_edge n' lh in
    (n', {g' with continue_set = BatSet.add n' g'.continue_set})
  | Return (_, _) ->
    let n' = Node.make () in
    (Node.exit, g |> add_node_stmt n' stmt |> add_edge n n' |> add_edge n' Node.exit)
  | Assume (BinOp (LAnd,e1,e2,_), loc) ->
    let (n1,g1) = trans (Assume (e1,loc)) lhop lxop (n,g) in
    let (n2,g2) = trans (Assume (e2,loc)) lhop lxop (n1,g1) in
    (n2,g2)
  | Assume (BinOp (LOr,e1,e2,einfo), loc) ->
    let (n1,g1) = trans (Assume (e1,loc)) lhop lxop (n,g) in
    let neg = Stmt.Assume (BinOp (LAnd, UnOp (LNot,e1,EType Bool), e2, einfo), loc) in
    let (n2,g2) = trans neg lhop lxop (n,g1) in (* should be n, not n1 *)
    let join = Node.make () in
    (join, g2 |> add_node_stmt join Skip |> add_edge n1 join |> add_edge n2 join)
  | Assume (UnOp (LNot, UnOp (LNot,e,_), _), loc) -> (* !!e -> e *)
    let stmt' = Stmt.Assume (e,loc) in
    trans stmt' lhop lxop (n,g)
  | Assume (UnOp (LNot, BinOp (LAnd,e1,e2,einfo), t), loc) -> (* !(e1 && e2) -> !e1 || !e2 *)
    let _ = assert (Typ.is_bool t) in
    let stmt' = Stmt.Assume (BinOp (LOr, UnOp (LNot,e1,t), UnOp (LNot,e2,t), einfo), loc) in
    trans stmt' lhop lxop (n,g)
  | Assume (UnOp (LNot, BinOp (LOr,e1,e2,einfo), t), loc) -> (* !(e1 || e2) -> !e1 && !e2 *)
    let _ = assert (Typ.is_bool t) in
    let stmt' = Stmt.Assume (BinOp (LAnd, UnOp (LNot,e1,t), UnOp (LNot,e2,t), einfo), loc) in
    trans stmt' lhop lxop (n,g)
  | Assume (_e,_loc) -> (* assumed to be an atomic predicate *)
    let n' = Node.make () in
    let g' = g |> add_node_stmt n' stmt |> add_edge n n' in
    (n',g')
  | Assert (_e,_,_loc) -> (* assumed to be an atomic predicate *)
    let n' = Node.make () in
    let g' = g |> add_node_stmt n' stmt |> add_edge n n' in
    (n',g')
  | _ -> 
    let n' = Node.make () in
    let g' = g |> add_node_stmt n' stmt |> add_edge n n' in
    (n',g')

(* disconnect edges starting from
 * either of
 * an exit, exception (throw or revert), continue, break *)
let disconnect : Cfg.t -> Cfg.t
= fun g ->
  fold_edges (fun n1 n2 acc ->
    if Node.equal n1 Node.exit then
      remove_edge n1 n2 acc else
    if Cfg.is_exception_node n1 acc then
      acc |> remove_edge n1 n2 |> add_edge n1 Node.exit else (* normalize so that every function has exit node at the end. *)
    if Cfg.is_continue_node n1 acc && not (Cfg.is_loophead n2 acc) then
      remove_edge n1 n2 acc else
    if Cfg.is_break_node n1 acc && not (Cfg.is_loopexit n2 acc) then
      remove_edge n1 n2 acc
    else acc
  ) g g

let remove_unreach : Cfg.t -> Cfg.t
= fun g ->
  let onestep g nodes = BatSet.fold (fun n acc -> BatSet.union (BatSet.of_list (succ n g)) acc) nodes nodes in
  let reachable = Vocab.fix (onestep g) (BatSet.singleton Node.entry) in
  fold_node (fun n acc ->
    if BatSet.mem n reachable then acc
    else remove_node n acc
  ) g g

(* remove spurious loopheader *)
let inspect_lh : Cfg.t -> Cfg.t
= fun g ->
  fold_node (fun n acc ->
    if Cfg.is_loophead n acc then
      if List.length (pred n acc) = 1 then (* only precondition exists *)
        {acc with lh_set = BatSet.remove n acc.lh_set}
      else if List.length (pred n acc) >=2 then acc 
      else raise (Failure "should not exist")
    else acc
  ) g g

let unroll : Cfg.t -> Cfg.t
= fun g ->
  let next_node n =
    (match n with
     | Node.ENTRY | Node.EXIT -> raise (Failure "invalid input")
     | Node.Node id -> Node.Node (id+1))
  in
  let (untargeted_lhs, g) =
  fold_edges (fun n1 n2 (tbd_lhs,acc) ->
    if Cfg.is_loophead n2 acc then
      if Cfg.is_outer_pred_of_lh n1 acc then (tbd_lhs,acc)
      else
        let acc' = remove_edge n1 n2 acc in
        (* given a loopheader id 'n', its corresponding
         * loopexit id is 'n+1'.
         * See 'trans' function *)
        let loopexit = next_node n2 in 
        let acc'' = add_edge n1 loopexit acc' in
        (BatSet.add n2 tbd_lhs, acc'')
    else (tbd_lhs, acc) 
  ) g (BatSet.empty,g)
  in
  let g = {g with lh_set = BatSet.diff g.lh_set untargeted_lhs} in
  let _ = assert (not (Cfg.has_loop g)) in
  g

let rec double_loop (stmt : Stmt.t) : Stmt.t =
  match stmt with
  | Assign _ | Decl _ -> stmt
  | Seq (s1,s2) -> Seq (double_loop s1, double_loop s2)
  | Call _ | Extcall _ | Skip -> stmt
  | If (e,s1,s2,i) -> If (e, double_loop s1, double_loop s2, i)
  (* While (e) {s} ->
   * While (e) {s}; While (e) {s} ->
   * unroll each while-loop once. *)
  | While (e,s) ->
    let s' = double_loop s in
    Seq (While (e,s'), While (e,s'))
  | Break | Continue | Return _
  | Revert | Assume _ | Assert _ | Assembly _ | Placeholder -> stmt
  | Unchecked _ -> assert false
  | Label _ -> stmt

let convert (stmt : Stmt.t) : Cfg.t =
  let stmt = match !Options.mode with Exploit -> double_loop stmt | _ -> stmt in
  let (n,g) = trans stmt None None (Node.entry, Cfg.empty) in
  let g = add_edge n Node.exit g in
  let g = disconnect g in
  let g = remove_unreach g in
  let g = inspect_lh g in
  let g = match !Options.mode with Exploit -> unroll g | _ -> g in
  g

let run : Pgm.t -> Pgm.t
= fun pgm ->
  List.map (fun contract ->
    let upd =
      List.map (fun func ->
        let body = Func.body func in
        let g = convert body in
        let g = {g with signature = Func.fkey func} in 
        Field.fset Func.Fields.info func {func.info with cfg = g}
      )
    in
    Field.map Contract.Fields.funcs ~f:upd contract 
  ) pgm
