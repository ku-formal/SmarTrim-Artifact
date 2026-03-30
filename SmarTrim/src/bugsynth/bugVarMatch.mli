open Frontend
open Frontend.Lang

val print_map : (Var.t, Var.t) BatMap.t -> unit

(* check whether vars from an example can be matched with vars from a seed *)
val matched_for_all : Py.Object.t -> Var.t BatSet.t -> Var.t BatSet.t -> bool

(* generate var map. return false if matching failure occured. *)
val mk_map : Py.Object.t -> Var.t BatSet.t -> Var.t BatSet.t -> (bool * (Var.t, Var.t) BatMap.t)

val mk_map2 : Py.Object.t ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (bool * (Var.t, Var.t) BatMap.t)

val mk_map3 : Py.Object.t ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (Var.t BatSet.t * Var.t BatSet.t) ->
              (bool * (Var.t, Var.t) BatMap.t)

val replace_lv : (Var.t, Var.t) BatMap.t -> lv -> lv
val replace_e : (Var.t, Var.t) BatMap.t -> exp -> exp
val replace_s : (Var.t, Var.t) BatMap.t -> Stmt.t -> Stmt.t
