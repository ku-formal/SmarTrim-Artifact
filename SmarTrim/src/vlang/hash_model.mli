(** deal with cryptohashes *)

(** {b Example}

    @param vf symbolic state
    @param arg_exp_list arguments of keccak256 *)
val add_assumption_and_get_output_exp :
  Formula.vformula -> Formula.vexp list -> (Formula.vformula * Formula.vexp) option

val keccak256_s : Frontend.Var.t
