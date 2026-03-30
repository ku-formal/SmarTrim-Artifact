open Lang

val separator : string
val code_public_getter : int
val get_full_base_contracts : Pgm.t -> Contract.t -> Pgm.t
val copy : Pgm.t -> Pgm.t
val rename : Pgm.t -> Pgm.t
val rmskip : Pgm.t -> Pgm.t
val run : Pgm.t -> Pgm.t
