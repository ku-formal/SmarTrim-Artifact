type t

val bit_usage : t -> int
val mk : Frontend.Lang.Pgm.t -> t
val get_offset : t -> Frontend.Var.t -> Z.t
