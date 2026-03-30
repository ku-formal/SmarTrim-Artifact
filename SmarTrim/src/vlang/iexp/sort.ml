(**** phantom types ****)

type t_bool = [ `Bl ]
type t_ubv = [ `Ubv ]
type t_sbv = [ `Sbv ]
type t_string = [ `String ]
type t_uninterp = [ `Un ]
type ('dom, 'codom) t_array = [ `Array of 'dom * 'codom ]

(** Signedness of bitvec. *)
type sign = U  (** unsigned *) | S  (** signed *) [@@deriving show { with_path = false }]

type 'p t =
  | Bool : t_bool t
  | Ubv : t_ubv t
  | Sbv : t_sbv t
  | String : t_string t
  | Uninterp : t_uninterp t
  | Array : 'dom t * 'codom t -> ('dom, 'codom) t_array t
