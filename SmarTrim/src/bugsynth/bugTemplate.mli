open Frontend
open Frontend.Lang

type name = string
and input_param = Var.t
and ret_param = Var.t

(* Q. Why differentiate 'gvars' and 'lvars'? *)
(* A. To address imprecision of existing word embedding *)
(* E.g. sim ('owner','account') > sim ('to','account') *)

type at = name * gvars_seed * aa list
and gvars_seed = Var.t BatSet.t

and aa = (* abstract atom *)
  | A_ReplaceExp of Fkey.t * exp * exp
  | A_ReplaceAssign of Fkey.t * (lv * exp) * (lv * exp)
  | A_RemoveStmt of Fkey.t * Stmt.t
  | A_ApplyUnchecked of Fkey.t * Stmt.t (* only atomic stmt *)
  | A_RemoveModifier of Fkey.t * name
  | A_Remove_IO_Guard
  | A_Remove_ModCalls of name
  | A_Func of (name * input_param list * ret_param list * name list * Stmt.t * payable)

  | A_Decl of Fsig.t * Var.t * exp
  | A_Assert of Fsig.t * exp

and payable = bool

type ca =
  | C_ReplaceExp of Loc.t * exp * exp
  | C_ReplaceAssign of Loc.t * (lv * exp) * (lv * exp)
  | C_RemoveStmt of Loc.t * Stmt.t
  | C_RemoveModifier of Loc.t * Mod_call.t
  | C_ApplyUnchecked of Loc.t * Loc.t (* (starting loc, ending loc) *)
  | C_Decl of Loc.t * Var.t * exp
  | C_Assert of Loc.t * exp
  | C_Func of int * name * input_param list * ret_param list * string list * Stmt.t * bool
 
type ct = name * ca list

val to_string_at : at -> string
val to_string_ct : ct -> string

val eq_at : at -> at -> bool


(* return 'false' if failed *)
val apply : Solc.Ver.t -> string -> ct -> string list -> (bool * string list)
