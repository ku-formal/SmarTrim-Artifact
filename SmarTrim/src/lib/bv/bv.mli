type t = private { signed : bool; size : int; data : Z.t }

val mk : signed:bool -> size:int -> Z.t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val rem : t -> t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t
val shift_left : t -> int -> t
val shift_right : t -> int -> t
val resize : t -> int -> t
val change_signedness : bool -> t -> t
val to_int : t -> int
val of_int : signed:bool -> size:int -> int -> t
val to_string : t -> string
val to_hex_string : t -> string
val compare : t -> t -> int
val cmp : t -> t -> int
val equal : t -> t -> bool
val eq : t -> t -> bool
val lt : t -> t -> bool
val leq : t -> t -> bool
val gt : t -> t -> bool
val geq : t -> t -> bool
