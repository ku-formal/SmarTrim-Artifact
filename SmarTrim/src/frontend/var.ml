type t = string * Typ.t [@@deriving compare, equal]

let origin_name (s : string) : string = try fst (String.split s ~by:"__@") with Not_found -> s
let mk id typ : t = (id, typ)
let pp fmt ((id, t) : t) = Pp.pf fmt "%s.%a" id Typ.pp_short t
let show = [%show: t]
let org (x : t) : t = (origin_name (fst x), snd x)
let add_tag ?(sep = "$") ~tag ((x, t) : t) = (x ^ sep ^ tag, t)
let label_txid ~txid v : t = add_tag ~sep:"@T" ~tag:(Int.to_string txid) v

let label_txid_order ~txid ~order v : t =
  let v = label_txid ~txid v in
  add_tag ~sep:"@O" ~tag:(Int.to_string order) v
;;

let label_entry v : t = add_tag ~sep:"@" ~tag:"E" v

let label_entry_txid ~txid v : t =
  let v = label_entry v in
  label_txid ~txid v
;;

let is_txid_labeled ((x, _) : t) : bool = String.exists x "@T"
