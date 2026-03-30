let ctx = Z3.mk_context []

(** [a / b >= c <=> a / c >= b] *)
let f n =
  let a = Z3.BitVector.mk_const_s ctx "a" n in
  let b = Z3.BitVector.mk_const_s ctx "b" n in
  let c = Z3.BitVector.mk_const_s ctx "c" n in
  let zero = Z3.BitVector.mk_numeral ctx "0" n in
  let b_nonzero = Z3.Boolean.(mk_not ctx (mk_eq ctx b zero)) in
  let c_nonzero = Z3.Boolean.(mk_not ctx (mk_eq ctx c zero)) in
  let p1 = Z3.BitVector.(mk_uge ctx (mk_udiv ctx a b) c) in
  let p2 = Z3.BitVector.(mk_uge ctx (mk_udiv ctx a c) b) in
  let p = Z3.Boolean.(mk_not ctx (mk_iff ctx p1 p2)) in
  let s = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add s [ b_nonzero; c_nonzero; p ];
  match Z3.Solver.check s [] with
  | SATISFIABLE ->
    Pp.pr "%s\n%!" (Z3.Solver.get_model s |> Option.get |> Z3.Model.to_string);
    assert false
  | UNSATISFIABLE -> Z3.Solver.UNSATISFIABLE
  | UNKNOWN -> UNKNOWN
;;

(** [a * b / a == b <=> (-1) / a >= b] *)
let g n =
  let a = Z3.BitVector.mk_const_s ctx "a" n in
  let b = Z3.BitVector.mk_const_s ctx "b" n in
  let zero = Z3.BitVector.mk_numeral ctx "0" n in
  let intmax = Z3.BitVector.mk_numeral ctx "-1" n in
  let a_nonzero = Z3.Boolean.(mk_not ctx (mk_eq ctx a zero)) in
  let p1 = Z3.Boolean.mk_eq ctx Z3.BitVector.(mk_udiv ctx (mk_mul ctx a b) a) b in
  let p2 = Z3.BitVector.(mk_uge ctx (mk_udiv ctx intmax a) b) in
  let p = Z3.Boolean.(mk_not ctx (mk_iff ctx p1 p2)) in
  let s = Z3.Solver.mk_solver ctx None in
  Z3.Solver.add s [ a_nonzero; p ];
  match Z3.Solver.check s [] with
  | SATISFIABLE ->
    Pp.pr "%s\n%!" (Z3.Solver.get_model s |> Option.get |> Z3.Model.to_string);
    assert false
  | UNSATISFIABLE -> Z3.Solver.UNSATISFIABLE
  | UNKNOWN -> UNKNOWN
;;

let () =
  let f = if Sys.argv.(1) = "0" then f else if Sys.argv.(1) = "1" then g else invalid_arg "" in
  let list = if Sys.argv.(2) = "small" then List.range 10 `To 100 else [ 256 ] in
  let work i =
    let s1 = Sys.time () in
    let s = f i in
    let s2 = Sys.time () -. s1 in
    match s with
    | SATISFIABLE -> assert false
    | UNSATISFIABLE -> Pp.pr "PASSED: %i bytes, time passed: %fs\n%!" i s2
    | UNKNOWN -> Pp.pr "UNKNOWN: %i bytes, time passed: %fs\n%!" i s2
  in
  List.iter work list
;;
