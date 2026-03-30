type t = string * Typ.t [@@deriving compare, equal]

val origin_name : string -> string
val mk : string -> Typ.t -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val org : t -> t

(** [add_tag ?sep ~tag t] generates a variable with name [id v ^ sep ^ tag] and same type. *)
val add_tag : ?sep:string -> tag:string -> t -> string * Typ.t

(** Usually used in [Exploit] mode. {b Example:} [label_txid ~txid:4 "x__@22" = "x__@22@T4"] *)
val label_txid : txid:int -> t -> t

(** Usually used in [Exploit] mode. {b Example:}
    [label_txid_order ~txid:3 ~order:4 "x__@22" = "x__@22@T3@O4"] *)
val label_txid_order : txid:int -> order:int -> t -> t

(** Usually used in [Exploit] mode, to store entry-state variables. {b Example:}
    [label_entry "x__@22" = "x__@22@E"] *)
val label_entry : t -> t

(** Usually used in [Exploit] mode. {b Example:} [label_entry_txid 4 "x__@22" = "x__@22@E@T4"] *)
val label_entry_txid : txid:int -> t -> t

val is_txid_labeled : t -> bool
