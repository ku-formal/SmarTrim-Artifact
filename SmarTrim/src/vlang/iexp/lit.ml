open Phantom

type 'phantom t =
  | Bool : bool -> t_bool t
  | Bv : Z.t * ('sign, 'bit) t_bv -> ('sign, 'bit) t_bv t
  | String : string -> t_str t
