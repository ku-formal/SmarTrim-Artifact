open Frontend.Lang
open Vlang
open Options
open Semantics

let collect ?(reg_check = false) ?(extern_ctx = false) :
    Global.t -> Cfg.t -> vformula -> Paths.t -> Node.t -> Query.t list =
 fun global g vf path node ->
  if reg_check then Regression.collect_queries global vf path (Cfg.find_stmt node g)
  else
    let stmt = Cfg.find_stmt node g in

    (* check both plain/reentering contexts to avoid overfitting for RE_EL *)
    let re = if !Chk.re then Reentrancy.collect_queries ~extern_ctx global vf path g stmt else [] in
    let tx = if !Chk.tx then Tx_origin.collect_queries global vf path g node else [] in
    let leak = if !Chk.el then Leak.collect_queries vf path stmt else [] in
    let kill = if !Chk.su then Suicidal.collect_queries vf path stmt else [] in
    let assertion =
      if Chk.(!assert_ || !erc20) then Assertion.collect_queries global vf path stmt else []
    in
    let overflow =
      if Chk.(!io || !dz) then Overflow.collect_queries vf path (Cfg.find_stmt node g) else []
    in
    re @ tx @ leak @ kill @ assertion @ overflow
;;
