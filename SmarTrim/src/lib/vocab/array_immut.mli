(** Immutable arrays. Definitely an array, but immutability is ensured by encapsulation. *)

type 'a t

val length : 'a t -> int
val get : 'a t -> int -> 'a
val make : int -> 'a -> 'a t
val init : int -> (int -> 'a) -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val sub : 'a t -> int -> int -> 'a t
val copy : 'a t -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val unsafe_get : 'a t -> int -> 'a
val of_array : 'a array -> 'a t
