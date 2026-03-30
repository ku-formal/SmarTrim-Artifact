type t =
  | IO  (** integer over/underflow *)
  | DZ  (** division-by-zero *)
  | ASSERT  (** assertion violation *)
  | SU  (** suicidal *)
  | EL  (** ether-leak *)
  | ERC20  (** ERC20 standard violation *)
  | RE_EL  (** reentrancy-ether-leak *)
  | RE  (** reentrancy *)
  | TX_ORG  (** dangerous use of tx.origin *)
  | COV  (** path-coverage *)
  | NO_EFFECT  (** effectless assignment *)
  | ASSIGN_CONST  (** constant assignment *)
  | DEAD  (** deadcode *)
[@@deriving show { with_path = false }, compare, equal]

let of_string s =
  match s with
  | "IO" -> IO
  | "DZ" -> DZ
  | "ASSERT" -> ASSERT
  | "SU" | "KA" | "KILL" -> SU
  | "EL" | "ETH_LEAK" -> EL
  | "ERC20" -> ERC20
  | "RE_EL" -> RE_EL
  | "RE" -> RE
  | "TX_ORG" -> TX_ORG
  | "COV" -> COV
  | "NO_EFFECT" -> NO_EFFECT
  | "ASSIGN_CONST" -> ASSIGN_CONST
  | "DEAD" -> DEAD
  | _ -> Pp.invalid_argf "%s" s
;;
