(** Console interface. *)

val turn_on : unit -> unit
val turn_off : unit -> unit

(** set verbosity. *)
val set_verbosity : int -> unit

(** get verbosity. *)
val get_verbosity : unit -> int

(** Print the string at the 1st line of the [Say] workspace. *)
val say1 : string -> unit

(** Print the string at the 2nd line of the [Say] workspace. *)
val say2 : string -> unit

(** Print the string directly above the [Say] workspace. Newline is automatically inserted.

    trying to print by [say ~level s] is considered as no-op if [level > Say.verbosity]. *)
val say : lv:int -> string -> unit

(** Same as [Say.say] but uses [Format.printf]-like format. *)
val pp : lv:int -> ('a, Format.formatter, unit, unit) format4 -> 'a
