type e =
  | Contract of string  (** parameter : id *)
  | Enum of string  (** parameter : id *)
  | Address
  | AddressPayable
  | Bool
  | String
  | UInt of int
  | SInt of int
  | Bytes of int  (** fixed-size byte arrays *)
  | DBytes
[@@deriving show { with_path = false }, compare, equal]

type t =
  | ConstInt
  | ConstString of { value : string }
  | ConstReal
  | EType of e
  | Struct of string list
  | Mapping of e * t
  | Mapping2 of t * t  (** for internal modeling, does not occur directly in Solidity codes *)
  | Array of t * int option  (** type, (size)? *)
  | TupleType of t list
  | FuncType of t list * t list
  | Void  (** dummy type *)
[@@deriving show { with_path = false }, compare, equal]

let to_string_e elem_typ =
  match elem_typ with
  | Contract id -> "contract " ^ id
  | Enum id -> id
  | Address -> "address"
  | AddressPayable -> "address:payable"
  | Bool -> "bool"
  | String -> "string"
  | UInt n -> "uint" ^ string_of_int n
  | SInt n -> "int" ^ string_of_int n
  | Bytes n -> "bytes" ^ string_of_int n
  | DBytes -> "dbytes" (* dynamically-sized byte array *)
;;

let rec to_string typ =
  match typ with
  | ConstInt -> "int_const"
  | ConstReal -> "rational_const"
  | ConstString { value } -> Pp.spf {|stringlit("%s")|} value
  | EType etyp -> to_string_e etyp
  | Struct lst -> "struct " ^ string_of_list ~first:"" ~last:"" ~sep:"." id lst
  | Mapping (etyp, typ) -> "mapping(" ^ to_string_e etyp ^ " => " ^ to_string typ ^ ")"
  | Mapping2 (t1, t2) -> "mapping2(" ^ to_string t1 ^ " => " ^ to_string t2 ^ ")"
  | Array (typ, None) -> to_string typ ^ "[]"
  | Array (typ, Some n) -> to_string typ ^ "[" ^ string_of_int n ^ "]"
  | Void -> "void"
  | TupleType typs -> "Tuple" ^ string_of_list ~first:"(" ~last:")" ~sep:", " to_string typs
  | FuncType (typs, ret_typs) ->
    "function" ^ " "
    ^ string_of_list ~first:"(" ~last:")" ~sep:"," to_string typs
    ^ " "
    ^ string_of_list ~first:"returns(" ~last:")" ~sep:"," to_string ret_typs
;;

let to_solidity_string_e elem_typ =
  match elem_typ with
  | Contract id -> id
  | Enum id -> id
  | Address -> "address"
  | AddressPayable -> "address payable"
  | Bool -> "bool"
  | String -> "string"
  | UInt n -> "uint" ^ string_of_int n
  | SInt n -> "int" ^ string_of_int n
  | Bytes n -> "bytes" ^ string_of_int n
  | DBytes -> "bytes"
;;

let rec to_solidity_string typ =
  let f = to_solidity_string in
  match typ with
  | ConstInt -> "int_const"
  | ConstReal -> "rational_const"
  | ConstString _ -> "string"
  | EType etyp -> to_solidity_string_e etyp
  | Struct lst -> "struct " ^ string_of_list ~first:"" ~last:"" ~sep:"." Vocab.id lst
  | Mapping (etyp, typ) -> "mapping(" ^ to_solidity_string_e etyp ^ " => " ^ f typ ^ ")"
  | Mapping2 (t1, t2) -> "mapping(" ^ f t1 ^ " => " ^ f t2 ^ ")"
  | Array (typ, None) -> f typ ^ "[]"
  | Array (typ, Some n) -> f typ ^ "[" ^ string_of_int n ^ "]"
  | Void -> "void"
  | TupleType typs -> "Tuple" ^ string_of_list ~first:"(" ~last:")" ~sep:", " f typs
  | FuncType (typs, ret_typs) ->
    "function" ^ " "
    ^ string_of_list ~first:"(" ~last:")" ~sep:"," f typs
    ^ " "
    ^ string_of_list ~first:"returns(" ~last:")" ~sep:"," f ret_typs
;;

let pp_short_e fmt (e : e) : unit =
  let open Pp in
  match e with
  | Contract c -> pf fmt "C#%s" c
  | Enum e -> pf fmt "E#%s" e
  | Address -> pf fmt "a"
  | AddressPayable -> pf fmt "ap"
  | Bool -> pf fmt "b"
  | String -> pf fmt "s"
  | DBytes -> pf fmt "db"
  | UInt 256 -> pf fmt "u"
  | UInt n -> pf fmt "u%i" n
  | SInt 256 -> pf fmt "i"
  | SInt n -> pf fmt "i%i" n
  | Bytes b -> pf fmt "b%i" b
;;

let rec pp_short fmt (t : t) : unit =
  let open Pp in
  match t with
  | ConstInt -> pf fmt "ci"
  | ConstString _ -> pf fmt "cs"
  | ConstReal -> pf fmt "cr"
  | EType e -> pp_short_e fmt e
  | Struct _ -> pf fmt "S#"
  | Mapping (e, t) -> pf fmt "M<%a:%a>" pp_short_e e pp_short t
  | Mapping2 (dom, ran) -> pf fmt "M2<%a:%a>" pp_short dom pp_short ran
  | Array (ran, Some n) -> pf fmt "A<%a,%i>" pp_short ran n
  | Array (ran, None) -> pf fmt "dA<%a>" pp_short ran
  | TupleType l -> pf fmt "T<%a>" (seq ~sep:comma_nocut pp_short) l
  | FuncType (dom, ran) ->
    pf fmt "F<%a:%a>" (seq ~sep:comma_nocut pp_short) dom (seq ~sep:comma_nocut pp_short) ran
  | Void -> pf fmt "v"
;;

let u256_e = UInt 256
let u256_t = EType (UInt 256)
let is_uintkind t = match t with EType (UInt _) -> true | _ -> false
let is_uint256 t = t = EType (UInt 256)
let is_sintkind t = match t with EType (SInt _) -> true | _ -> false
let is_mapping t = match t with Mapping _ -> true | _ -> false
let is_mapping2 t = match t with Mapping2 _ -> true | _ -> false
let is_struct t = match t with Struct _ -> true | _ -> false
let is_enum t = match t with EType (Enum _) -> true | _ -> false
let is_elem t = match t with EType _ -> true | _ -> false
let is_usual_mapping t = t = Mapping (Address, EType (UInt 256))
let is_usual_allowance t = t = Mapping (Address, Mapping (Address, EType (UInt 256)))
let is_bool t = t = EType Bool
let is_address t = t = EType Address
let is_address_payable t = t = EType AddressPayable
let is_address_kind t = is_address t || is_address_payable t
let is_array t = match t with Array _ -> true | _ -> false
let is_static_array t = match t with Array (_, Some _) -> true | _ -> false

let get_array_size t =
  match t with
  | Array (_, Some n) -> Some n
  | Array (_, None) -> None
  | _ -> failwith "get_array_size"
;;

let is_dynamic_array t = match t with Array (_, None) -> true | _ -> false
let is_contract t = match t with EType (Contract _) -> true | _ -> false

let to_contract_name_exn t : string =
  match t with
  | EType (Contract c) -> c
  | _ -> Pp.invalid_argf "Typ.to_contract_name_exn : %a is not a valid contract type" pp t
;;

let is_user_defined t =
  match t with EType (Enum _) | EType (Contract _) | Struct _ -> true | _ -> false
;;

let is_dbytes t = match t with EType DBytes -> true | _ -> false
let is_bytes t = match t with EType (Bytes _) -> true | _ -> false
let is_bytekind t = is_dbytes t || is_bytes t
let is_const_int t = t = ConstInt
let is_uintkind_or_constint t = is_uintkind t || is_const_int t
let is_const_string t = match t with ConstString _ -> true | _ -> false
let is_string t = t = EType String
let is_tuple t = match t with TupleType _ -> true | _ -> false
let is_func typ = match typ with FuncType _ -> true | _ -> false
let is_void t = t = Void

let domain t =
  match t with
  | Array _ -> u256_t
  | Mapping (et, _) -> EType et
  | Mapping2 (t, _) -> t
  | EType DBytes -> u256_t
  | EType (Bytes _) -> u256_t
  | _ -> failwith "domain_typ"
;;

let range t =
  match t with
  | Array (t, _) -> t
  | Mapping (_, t) -> t
  | Mapping2 (_, t) -> t
  | EType DBytes -> EType (Bytes 1)
  | EType (Bytes _) -> EType (Bytes 1)
  | _ -> Pp.failwithf "Frontend.Typ.range : %a" pp t
;;

(** {b Example} [mapping(uint=>mapping(bool=>uint[3]))] -> [([uint, bool, uint], uint)]

    @param ?assert_no_mapping2 (=[true]) if matched with Mapping2, then suicide *)
let domain_range_chain ?(assert_no_mapping2 = true) t : t list * t =
  let rec aux acc t =
    match t with
    | Array (codom, _) -> aux (u256_t :: acc) codom
    | Mapping (dom, codom) -> aux (EType dom :: acc) codom
    | Mapping2 (dom, codom) ->
      assert (not assert_no_mapping2);
      aux (dom :: acc) codom
    | _ -> t :: acc
  in
  let l = aux [] t in
  let hd, tl = (List.hd l, List.tl l) in
  (tl |> List.rev, hd)
;;

(** {b Example} [mapping(uint=>struct X)] -> [Some (struct X)], [struct X] -> [None],
    [mapping(uint=>mapping(uint=>struct X))] -> [Some (struct X)] *)
let rec get_codomain_if_struct_container t : t option =
  match t with
  | Array (Struct l, _) -> Some (Struct l)
  | Mapping (_, Struct l) -> Some (Struct l)
  | Array (t, _) -> get_codomain_if_struct_container t
  | Mapping (_, t) -> get_codomain_if_struct_container t
  | _ -> None
;;

let get_func_ret t = match t with FuncType (_, rt) -> rt | _ -> failwith "get_func_ret_typs"
let tuple_elem t = match t with TupleType lst -> lst | _ -> failwith "tuple_elem_typs"
let get_bytes_size t = match t with EType (Bytes n) -> n | _ -> failwith "get_bytes_size"
let get_array_elem t = match t with Array (t, _) -> t | _ -> failwith "get_type_array_elem"

(** TODO: consider separation *)
let get_name_userdef t =
  match t with
  | Struct lst -> string_of_list ~first:"" ~last:"" ~sep:"." id lst
  | EType (Enum s) -> s
  | EType (Contract s) -> s
  | _ -> failwith ("get_name_userdef : " ^ to_string t)
;;

let preceding (t1 : t) (t2 : t) : t =
  if t1 = t2 then t1
  else
    match (t1, t2) with
    | EType String, ConstString _ -> t1
    | ConstString _, EType String -> t2
    | EType (UInt n1), EType (UInt n2) -> EType (UInt (max n1 n2))
    | EType (SInt n1), EType (SInt n2) -> EType (SInt (max n1 n2))
    | EType (SInt n1), EType (UInt n2) when n1 > n2 -> t1
    | EType (SInt n1), EType (UInt n2) when n1 <= n2 ->
      failwith "preceding_typ : intX1 and uintX2 are not convertible if X1<=X2"
    | EType (UInt n1), EType (SInt n2) when n1 < n2 -> t2
    | EType (UInt n1), EType (SInt n2) when n1 >= n2 -> t1
    | ConstInt, EType AddressPayable -> t2
    | ConstInt, EType Address -> t2
    | ConstInt, EType (Contract _) -> t2
    | EType Address, ConstInt -> t1
    | EType AddressPayable, ConstInt -> t1
    | EType (UInt 256), ConstInt -> t1
    | ConstInt, EType (UInt 256) -> t2
    | EType (Contract _), ConstInt -> t1
    | EType Address, EType (Contract _) -> t1
    | EType (Contract _), EType Address -> t2
    | EType (Bytes _), ConstInt -> t1
    | ConstInt, EType (Bytes _) -> t2 (* e.g., line 287 in zeus/024.sol *)
    | Array (t1, None), Array (t2, Some _) when t1 = t2 ->
      Array (t1, None) (* XXX: this is obviously incorrect. *)
    | EType (Contract _id1), EType (Contract _id2) -> t2
    | ConstString _, EType (Bytes _) -> t2
    | ConstString _, EType DBytes -> t2
    | EType (Bytes _), ConstString _ -> t1
    | EType DBytes, ConstString _ -> t1
    | EType Address, EType AddressPayable -> t1
    | EType AddressPayable, EType Address -> t2
    | Struct _, EType (UInt n) when n > 256 -> t2
    | EType (UInt n), Struct _ when n > 256 -> t1
    | t1, t2 -> failwith ("preceding_typ : " ^ to_string t1 ^ " vs. " ^ to_string t2)
;;

let rec is_implicitly_convertible (my_typ : t) (comp_typ : t) : bool =
  if my_typ = comp_typ then true
  else
    match (my_typ, comp_typ) with
    | ConstInt, EType (UInt _) -> true
    | ConstInt, Struct _ -> false
    | EType (UInt n1), EType (UInt n2) -> n1 <= n2
    | ConstInt, EType (SInt _) -> true
    | EType (SInt n1), EType (SInt n2) -> n1 <= n2
    | EType (SInt _), EType (UInt _) -> false (* negative numbers cannot be unsigned numbers *)
    | ConstInt, EType Address -> true
    | EType AddressPayable, EType Address -> true
    | EType Address, EType AddressPayable -> false
    | ConstInt, EType String -> false
    | ConstInt, Array _ -> false
    | EType (Contract _), EType Address ->
      (* true for 0.4x, false for later versions *)
      if Solc.Ver.get_major_minor Options.(get_solc_ver ()) = (0, 4) then true else false
    | EType (Contract _), EType (Bytes _) -> false
    | EType (Contract _), EType DBytes -> false
    | ConstString _, EType (Bytes _) -> true
    | ConstString _, EType DBytes -> true
    | ConstString _, EType String -> true
    | ConstString _, Array (EType String, _) -> false
    | ConstString _, Array (EType DBytes, _) -> false
    | ConstString _, EType (UInt _) -> false
    | ConstString _, EType (Contract _) -> false
    | ConstString _, EType Address -> false
    | EType String, EType DBytes -> false
    | EType String, EType (Bytes _) -> false
    | EType String, Struct _ -> false
    | Struct _, EType String -> false
    | EType DBytes, EType String -> false
    | EType (Bytes n1), EType (Bytes n2) -> n1 <= n2
    | EType (Bytes _), EType String -> false
    | EType (Bytes _), EType DBytes -> false
    | EType (Bytes _), EType (Contract _) -> false
    | EType DBytes, EType (Bytes _) -> false
    | EType DBytes, EType Address -> false
    | EType Address, EType (Contract _) -> false
    | EType Address, EType (Enum _) -> false
    | EType (UInt n), EType Address ->
      if Solc.Ver.get_major_minor Options.(get_solc_ver ()) = (0, 4) then
        if n <= 160 then true else false
      else false
    | EType Address, EType (UInt _n) -> false
    | EType (SInt _n), EType Address -> false
    | EType Address, EType (SInt _n) -> false
    | EType Address, Array _ -> false
    | Array _, EType Address -> false
    | EType (UInt _), EType String -> false
    | EType String, EType (UInt _) -> false
    | EType DBytes, EType (UInt _) -> false
    | Array (t1, Some n1), Array (t2, Some n2) when n1 = n2 -> is_implicitly_convertible t1 t2
    | Array (t1, None), Array (t2, None) -> is_implicitly_convertible t1 t2
    | Array _, Array _ -> false
    | Struct lst1, Struct lst2 ->
      if not (List.length lst1 = List.length lst2) then false
      else List.for_all2 (fun i1 i2 -> i1 = i2) lst1 lst2
    | EType (Contract _), EType (UInt _) -> false
    | EType (Bytes _), Struct _ -> false
    | Struct _, EType (UInt _) -> false
    | Struct _, EType (SInt _) -> false
    | EType (UInt _), Struct _ -> false
    | EType (UInt _), EType (Contract _) -> false
    | EType (Enum id1), EType (Enum id2) -> String.equal id1 id2
    | EType (Enum _), EType Address -> false
    | Struct _, EType Address ->
      false (* TODO: consider inheritance order; convertible if child -> parent *)
    | EType (Contract _c1), EType (Contract _c2) -> true
    | EType (UInt n1), EType (SInt n2) -> n1 < n2
    | ConstInt, EType (Bytes _) -> true
    | EType (UInt _), EType DBytes -> false
    | EType (UInt _), EType (Bytes _) -> false
    | EType (Bytes _), EType (UInt _) -> false
    | EType (Bytes _), EType Address -> false
    | EType Address, EType (Bytes _) -> false
    | EType Address, EType DBytes -> false
    | EType _, Array _ -> false
    | Array _, EType _ -> false
    | EType Bool, EType Address -> false
    | EType Address, EType Bool -> false
    | EType Bool, EType String -> false
    | EType String, EType Bool -> false
    | EType Bool, EType (UInt _) -> false
    | EType (UInt _), EType Bool -> false
    | EType Bool, EType (Bytes _) -> false
    | EType (Bytes _), EType Bool -> false
    | EType Bool, EType DBytes -> false
    | EType DBytes, EType Bool -> false
    | EType String, EType Address -> false
    | EType Address, EType String -> false
    | Struct _, EType (Bytes _) -> false
    | EType DBytes, EType (Enum _) -> false
    | Struct _, Array _ -> false
    | EType (SInt _), Struct _ -> false
    | _ ->
      failwith ("is_implicitly_convertible : " ^ to_string my_typ ^ " vs. " ^ to_string comp_typ)
;;

let print_typ oc (typ : t) = Printf.fprintf oc "%s" (to_string typ)

let print_etyp oc (etyp : e) =
  let open Printf in
  match etyp with
  | Contract id -> fprintf oc "contract_%s" id
  | Enum id -> fprintf oc "enum_%s" id
  | Address -> fprintf oc "address"
  | AddressPayable -> fprintf oc "addressPayable"
  | Bool -> fprintf oc "bool"
  | String -> fprintf oc "string"
  | DBytes -> fprintf oc "dbytes"
  | UInt n -> fprintf oc "uint%i" n
  | SInt n -> fprintf oc "int%i" n
  | Bytes n -> fprintf oc "bytes%i" n
;;

(** Used to calculate how large the struct pointer space should have. For example, if we want to
    allocate a struct pointer space for a global variable [mapping(uint=>struct X) m], we need
    [2 ** 256] new struct pointers.

    {b Example}
    [uint -> -1, struct X -> 0, mapping(uint=>struct X) -> 1, mapping(uint=>struct X[3]) -> 2] *)
let struct_score t =
  let rec aux acc t =
    match t with Struct _ -> acc | Mapping (_, t) | Array (t, _) -> aux (acc + 1) t | _ -> -1
    (* ignore Mapping2, they are not solidity-natural structures *)
  in
  aux 0 t
;;
