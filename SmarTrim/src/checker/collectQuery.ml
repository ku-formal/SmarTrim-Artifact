open Options
open Frontend.Lang
open Semantics
open Vlang

let collect ~internal_tx (global : Global.t) (g : Cfg.t) (vf : vformula) (path : Paths.t)
    (stmt : Stmt.t) : Query.t list =
  let re =
    if !Chk.re && internal_tx then Reentrancy.collect_queries ~extern_ctx:true global vf path g stmt
    else []
  in
  let el = if !Chk.el then Leak.collect_queries vf path stmt else [] in
  let su = if !Chk.su then Suicidal.collect_queries vf path stmt else [] in
  let assertion =
    if Chk.(!assert_ || !erc20) then Assertion.collect_queries global vf path stmt else []
  in
  let overflow = if Chk.(!io || !dz) then Overflow.collect_queries vf path stmt else [] in
  re @ el @ su @ assertion @ overflow
;;
