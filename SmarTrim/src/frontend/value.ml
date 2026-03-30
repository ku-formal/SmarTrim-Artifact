type t =
  | Bool of bool
  | Bv of Bv.t
  | Int of Z.t
  | Frac of Q.t
  | Str of string
  | Map of Typ.e * t  (** domain * initial value, a constant array *)
[@@deriving variants]

let rec typ (v : t) : Typ.t =
  match v with
  | Bool _ -> EType Bool
  | Bv bv -> if bv.signed then EType (SInt bv.size) else EType (UInt bv.size)
  | Int _ -> ConstInt
  | Frac _ -> ConstReal
  | Str _ -> EType String
  | Map (e, v) -> Mapping (e, typ v)
;;

let rec pp ppf (v : t) : unit =
  match v with
  | Bool b -> Pp.bool ppf b
  | Bv bv -> Pp.string ppf (Bv.to_string bv)
  | Int n -> Z.pp ppf n
  | Frac q -> Q.pp_print ppf q
  | Str s -> Pp.string ppf s
  | Map (t, m) -> Pp.pf ppf "K(%a, %a)" Typ.pp_e t pp m
;;

let to_string v = Pp.str pp v
