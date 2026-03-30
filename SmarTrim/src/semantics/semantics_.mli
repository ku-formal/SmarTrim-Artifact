open Frontend.Lang
open Vlang

val reset_rid : unit -> unit
val rename : vexp -> vexp
val is_renamed : string -> bool
val convert_aexp : exp -> vexp
val convert_bexp : exp -> vformula
val convert_lv : lv -> vexp

val convert_stmt :
  Global.t -> Func.t -> Stmt.t -> vformula * Query.t list -> vformula * Query.t list
