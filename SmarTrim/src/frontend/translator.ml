open Lang
module S = String
module Y = Yojson.Basic
module J = Yojson.Basic.Util

let end_of_lines = ref [ -1 ]

(** store cumulative byte size at the end of each line *)
let record_end_of_lines (lines : string list) =
  let _, lst =
    List.fold_left
      (fun (acc_eol, acc_lst) cur ->
        let eol = Bytes.length (Bytes.of_string cur) + 1 in
        let acc_eol' = eol + acc_eol in
        (acc_eol', acc_lst @ [ acc_eol' ]))
      (0, []) lines
  in
  end_of_lines := lst
;;

let line_indicator (lst : int list) (byte : int) : int =
  List.fold_left
    (fun (set, line) cur ->
      if (not set) && byte < cur then (true, line) else if set then (set, line) else (set, line + 1))
    (false, 1) lst
  |> fun (_, n) -> n
;;

let get_loc (json : Y.t) : Loc.t =
  let lst = S.split_on_char ':' (json |> J.member "src" |> J.to_string) in
  let offset, len = (int_of_string (List.at lst 0), int_of_string (List.at lst 1)) in
  {
    line = line_indicator !end_of_lines offset;
    finish_line = line_indicator !end_of_lines (offset + len);
    offset;
    len;
  }
;;

let get_id (json : Y.t) : int = json |> J.member "id" |> J.to_int

(******************************)
(******************************)
(***** Type-related parts *****)
(******************************)
(******************************)

let elem_typs =
  [
    "address";
    "address payable";
    "contract";
    "library";
    "enum";
    "bool";
    "int";
    "uint";
    "byte";
    "fixed";
    "ufixed";
    "bytes";
    "string";
  ]
;;

(******************************)
(*** typeIdentifier to type ***)
(******************************)

let rec trans_typeIdentifier (type_string_all : string) (type_identifier : string) : Typ.t =
  let preprocess str =
    (str |> fun res -> if S.starts_with res "_" then S.lchop ~n:1 res else res) |> fun res ->
    if S.ends_with res "_" then S.rchop ~n:1 res else res
  in
  let get_tups lst =
    if List.length lst = 1 && List.hd lst = "__" then []
    else lst |> List.map preprocess |> List.map (trans_typeIdentifier type_string_all)
  in
  match type_identifier with
  | _ when S.starts_with type_identifier "t_stringliteral" -> trans_typeString type_string_all
  | "t_address_payable" -> EType AddressPayable
  | "t_address" -> EType Address
  | _ when S.starts_with type_identifier "t_uint" ->
    EType (UInt (int_of_string (S.tail type_identifier 6)))
  | _ when S.starts_with type_identifier "t_int" ->
    EType (SInt (int_of_string (S.tail type_identifier 5)))
  | "t_bool" -> EType Bool
  | _ when S.starts_with type_identifier "t_bytes_" -> EType DBytes
  | _ when S.starts_with type_identifier "t_bytes" ->
    EType (Bytes (int_of_string (S.lchop ~n:7 type_identifier)))
  | _ when S.starts_with type_identifier "t_contract" ->
    let lst = S.split_on_char '$' type_identifier in
    let cname = S.chop ~l:1 ~r:1 (List.at lst 1) in
    (* _ContractName_ => ContractName *)
    EType (Contract cname)
  | _ when S.starts_with type_identifier "t_string_" -> EType String
  | _ when S.starts_with type_identifier "t_tuple" ->
    let str =
      assert (S.ends_with type_identifier "$");
      S.rchop ~n:1 type_identifier
    in
    let lst = S.split_on_char '$' str |> List.tl |> get_tups in
    TupleType lst
  | _ when S.starts_with type_identifier "t_function" ->
    let str =
      (* remove "bound_to ... " - unnecessary to identify return typ *)
      if S.exists type_identifier "$bound_to" then
        S.left type_identifier (S.find type_identifier "$bound_to" + 1)
      else type_identifier
    in
    let str =
      let str = snd (S.replace ~str ~sub:"$value_$" ~by:"$") in
      (* e.g., simple_dao.sol *)
      let str = snd (S.replace ~str ~sub:"$value$" ~by:"$") in
      snd (S.replace ~str ~sub:"$value" ~by:"$")
    in
    let str =
      assert (S.ends_with str "$");
      S.rchop ~n:1 str
    in
    let lst = S.split_on_char '$' str in
    let ret_idx =
      if
        S.starts_with str "t_function_setvalue_nonpayable$" (* e.g., simple_dao.sol *)
        || S.starts_with str "t_function_setvalue_pure$" (* solv 0.5 *)
      then match List.index_of "returns" lst with Some i -> i | None -> assert false
      else match List.rindex_of "returns" lst with Some i -> i | None -> assert false
    in
    (* ["t_function_..."; "_t_..._"; "returns"; "__"] => (["t_function_..."; "_t_..._"], ["returns"; "__"]) *)
    let ilst, olst = List.split_at ret_idx lst in
    let ilst, olst = (List.tl ilst, List.tl olst) in
    (* ["_t_..._"], ["__"] *)

    let rec recover lst =
      (* join strings that should be consecutive *)
      match lst with
      | [] -> []
      | h :: [] -> [ h ]
      | "_t_array" :: _ ->
        let rec find_arr_size_idx depth idx lst =
          match lst with
          | [] -> assert false
          | "_t_contract" :: _name :: _cid :: tl -> find_arr_size_idx depth (idx + 3) tl
          | h :: tl when not (S.starts_with h "_") ->
            if depth = 1 then idx (* solution found *)
            else
              let _ = assert (depth > 1) in
              find_arr_size_idx (depth - 1) (idx + 1) tl
          | "_t_array" :: tl -> find_arr_size_idx (depth + 1) (idx + 1) tl
          | "_t_struct" :: tl -> find_arr_size_idx (depth + 1) (idx + 1) tl
          | _ :: tl -> find_arr_size_idx depth (idx + 1) tl
        in
        let size_idx = find_arr_size_idx 0 0 lst in
        let arr, tl' = List.split_at (size_idx + 1) lst in
        (* find 'arr' including the size part *)
        let h' = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id arr in
        h' :: recover tl'
      | "_t_contract" :: cname :: cid :: tl ->
        let h' = "_t_contract" ^ "$" ^ cname ^ "$" ^ cid in
        h' :: recover tl
      | "_t_struct" :: sname :: sid :: tl ->
        let h' = "_t_struct" ^ "$" ^ sname ^ "$" ^ sid in
        h' :: recover tl
      | "_t_enum" :: ename :: eid :: tl ->
        let h' = "_t_enum" ^ "$" ^ ename ^ "$" ^ eid in
        h' :: recover tl
      | "_t_function_barecall_payable" :: "_t_bytes_memory_ptr_" :: "returns" :: "_t_bool_"
        :: "_t_bytes_memory_ptr_" :: tl ->
        let h' =
          "_t_function_barecall_payable$_t_bytes_memory_ptr_$returns$_t_bool_$_t_bytes_memory_ptr_$"
        in
        h' :: recover tl
      | "_t_function_barecall_payable" :: "__" :: "returns" :: "_t_bool_" :: tl ->
        (* solc < 0.5.0 *)
        let h' = "_t_function_barecall_payable$__$returns$_t_bool_$" in
        h' :: recover tl
      (* _t_function_external_payable$_t_uint256_$_t_string_memory_ptr_$_t_string_memory_ptr_$returns$_t_bytes32_$value_$ *)
      | "_t_function_external_payable" :: _ ->
        let rec find_end_idx idx lst =
          match lst with
          | [] -> idx (* 'value_' has been removed by the front routine *)
          (* | "value_"::tl -> idx *)
          | _ :: tl -> find_end_idx (idx + 1) tl
        in
        let end_idx = find_end_idx 0 lst in
        let func, tl' = List.split_at end_idx lst in
        (* including 'end_idx' *)
        let h' = Vocab.string_of_list ~first:"" ~last:"$" ~sep:"$" Vocab.id func in
        h' :: recover tl'
      | "_t_function_internal_view" :: _ ->
        let rec find_end_idx idx lst =
          match lst with
          | [] -> assert false
          | "_" :: _ -> idx
          | _ :: tl -> find_end_idx (idx + 1) tl
        in
        let end_idx = find_end_idx 0 lst in
        let func, tl' = List.split_at end_idx lst in
        let h' = Vocab.string_of_list ~first:"" ~last:"$" ~sep:"$" Vocab.id func in
        h' :: recover tl'
      | h :: tl -> h :: recover tl
    in
    FuncType (get_tups (recover ilst), get_tups (recover olst))
  | _ when S.starts_with type_identifier "t_mapping" ->
    let str =
      assert (S.ends_with type_identifier "$");
      S.rchop ~n:1 type_identifier
    in
    let lst = S.split_on_char '$' str in
    assert (List.length lst = 3);
    let k, v = (List.at lst 1, List.at lst 2) in
    assert (S.starts_with k "_" && S.ends_with k "_");
    assert (S.starts_with v "_" && S.ends_with v "_");
    let k, v = (S.chop ~l:1 ~r:1 k, S.chop ~l:1 ~r:1 v) in
    let k = match trans_typeIdentifier type_string_all k with EType t -> t | _ -> assert false in
    Mapping (k, trans_typeIdentifier type_string_all v)
  (* e.g: "t_array$_t_array$_t_uint256_$3_memory_$5_memory_ptr" => uint256[3][5] *)
  | _ when S.starts_with type_identifier "t_array" ->
    let lst = S.split_on_char '$' type_identifier in
    (* e.g: t_array$_t_uint256_$dyn_memory_ptr *)
    let typ_part_lst = lst |> List.tl |> fun res -> List.remove_at (List.length res - 1) res in
    let typ_str = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id typ_part_lst in
    assert (S.starts_with typ_str "_" && S.ends_with typ_str "_");
    let typ_str = S.chop ~l:1 ~r:1 typ_str in
    let size_str = List.last lst in
    let typ = trans_typeIdentifier type_string_all typ_str in
    let size =
      match size_str with
      | _ when S.starts_with size_str "dyn_" -> None
      | _ -> Some (S.split_on_char '_' size_str |> (fun res -> List.at res 0) |> int_of_string)
    in
    Array (typ, size)
  | _ when S.starts_with type_identifier "t_struct" ->
    (* $_t_struct$_TokenOwnership_$1418_memory_ptr_$ *)
    let lst = S.split_on_char '$' type_identifier in
    let sname = List.at lst 1 in
    assert (S.starts_with sname "_" && S.ends_with sname "_");
    let sname = S.chop ~l:1 ~r:1 sname in
    (* TokenOwnership *)
    let ts_all_lst =
      let ts_all =
        S.replace_chars
          (fun c -> match c with ',' | '(' | ')' -> " " | _ -> S.of_char c)
          type_string_all
      in
      let ts_all = S.nreplace ~str:ts_all ~sub:"  " ~by:" " in
      S.split_on_char ' ' ts_all
    in
    let sname = ts_all_lst |> List.find_all (fun s -> S.ends_with s sname) |> Set.of_list in
    let sname =
      assert (Set.cardinal sname = 1);
      Set.choose sname
    in
    if not (S.exists sname ".") then Struct [ sname ] (* global struct *)
    else
      let cname, sname = S.split sname ~by:"." in
      Struct [ cname; sname ]
  | _ when S.starts_with type_identifier "t_enum" ->
    (* "t_enum$_RecoverError_$875" *)
    let lst = S.split_on_char '$' type_identifier in
    let ename = List.at lst 1 in
    assert (S.starts_with ename "_" && S.ends_with ename "_");
    let ename = S.chop ~l:1 ~r:1 ename in
    (* RecoverError *)
    let ts_all_lst =
      (* To handle "function (uint256, uint256) pure returns (enum CarefulMath.MathError,uint256)" *)
      let ts_all =
        S.replace_chars
          (fun c -> match c with ',' | '(' | ')' -> " " | _ -> S.of_char c)
          type_string_all
      in
      let ts_all = S.nreplace ~str:ts_all ~sub:"  " ~by:" " in
      S.split_on_char ' ' ts_all
    in
    let ename = ts_all_lst |> List.find_all (fun s -> S.ends_with s ename) |> Set.of_list in
    let ename =
      assert (Set.cardinal ename = 1);
      Set.choose ename
    in
    if not (S.exists ename ".") then EType (Enum ename)
    else
      let _, ename = S.split ename ~by:"." in
      EType (Enum ename)
  | _ when S.starts_with type_identifier "t_type$_t_super" -> Void (* 'super' keyword *)
  | _ when S.starts_with type_identifier "t_type" ->
    (* type conversion case *)
    let lst = S.split_on_char '$' type_identifier in
    (* e.g: "t_type$_t_string_storage_ptr_$", "t_type$_t_contract$_IERC721_$125_$" *)
    let arg_part_lst = lst |> List.tl in
    (* ["_t_contract"; "_IERC721_"; "125_"] *)
    let arg_str = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id arg_part_lst in
    (* _t_contract$_IERC721_$125_ *)
    assert (S.starts_with arg_str "_" && S.ends_with arg_str "_");
    let arg_str = S.chop ~l:1 ~r:1 arg_str in
    (* t_contract$_IERC721_$125 *)
    trans_typeIdentifier type_string_all arg_str
  | "t_magic_message" -> Void (* msg *)
  | _ ->
    Pp.pr "[WARNING] type conversion : %s (%s)\n%!" type_identifier type_string_all;
    Void

and trans_typeString (str : string) : Typ.t =
  let str = if S.exists str "type(" then S.chop ~l:5 ~r:1 str else str in
  let str = S.nreplace ~str ~sub:" storage " ~by:" " in
  let str = S.nreplace ~str ~sub:" ref" ~by:"" in
  let str = S.nreplace ~str ~sub:" memory" ~by:"" in
  let str = S.nreplace ~str ~sub:" calldata" ~by:"" in
  let str = S.nreplace ~str ~sub:" super " ~by:"" in
  match str with
  | _ when S.ends_with (S.nreplace ~str ~sub:" pointer" ~by:"") "]" -> trans_array str
  | _ when S.starts_with str "int_const" -> ConstInt
  | _ when S.starts_with str "rational_const" -> ConstReal
  | _ when S.starts_with str "literal_string" ->
    (* format: "literal_string \"123\"" *)
    let value = S.chop ~l:16 ~r:1 str in
    ConstString { value }
  | _ when S.starts_with str "tuple" -> trans_tuple str
  | _ when S.starts_with str "struct" -> trans_struct_type str
  | _ when S.starts_with str "mapping" -> trans_mapping str
  | _ when List.exists (fun e -> S.starts_with str e) elem_typs ->
    EType (trans_elementaryTypeName str)
  | _ -> Void

and trans_elementaryTypeName (str : string) : Typ.e =
  match str with
  | _ when S.starts_with str "contract" ->
    let _, str = S.replace ~str ~sub:"contract " ~by:"" in
    Contract str
  | _ when S.starts_with str "library" ->
    let _, str = S.replace ~str ~sub:"library " ~by:"" in
    Contract str
  | _ when S.starts_with str "enum" -> (
    match trans_enum_type str with EType (Enum ename) -> Enum ename | _ -> assert false)
  | _ when S.exists str "string" -> String
  | "address" -> Address
  | "address payable" -> AddressPayable
  | "bool" -> Bool
  | s when S.starts_with s "uint" ->
    let n_str = S.tail s 4 in
    if S.equal n_str "" then UInt 256 else UInt (int_of_string n_str)
  | s when S.starts_with s "int" ->
    let n_str = S.tail s 3 in
    if S.equal n_str "" then SInt 256 else SInt (int_of_string n_str)
  | "byte" -> Bytes 1
  | s when S.starts_with s "bytes" ->
    let n_str = S.tail s 5 in
    if S.equal n_str "" || S.exists s " " then DBytes else Bytes (int_of_string n_str)
  (* currently, (u)fixed point numbers are unstable in Solidity. *)
  | "fixed" -> failwith "Unsupported: fixed"
  | "ufixed" -> failwith "Unsupported: ufixed"
  | s -> failwith ("Unsupported: trans_elementraryTypeName - " ^ s)

and trans_struct_type (str : string) : Typ.t =
  assert (S.starts_with str "struct");
  let _, str' = S.replace ~str ~sub:" pointer" ~by:"" in
  let _, final = S.replace ~str:str' ~sub:"struct " ~by:"" in
  (* struct Name => Name *)
  if not (S.exists final ".") then Struct [ final ] (* global struct *)
  else
    let cname, sname = S.split final ~by:"." in
    Struct [ cname; sname ]

and trans_enum_type (str : string) : Typ.t =
  assert (S.starts_with str "enum");
  let _, str' = S.replace ~str ~sub:" pointer" ~by:"" in
  let _, final = S.replace ~str:str' ~sub:"enum " ~by:"" in
  (* struct tmp.tmp1 => tmp.tmp1 *)
  if not (S.exists final ".") then EType (Enum final)
  else
    let _, ename = S.split final ~by:"." in
    EType (Enum ename)

and trans_mapping (str : string) : Typ.t =
  assert (S.exists str "mapping");
  if S.ends_with str "]" then trans_array str
  else
    let left, right = S.split str ~by:" => " in
    (* "mapping(key => val)" -> ( "mapping(key", "val)" )*)
    let left' = S.lchop ~n:8 left in
    (* "mapping(key" -> key *)
    let right' = S.rchop ~n:1 right in
    (* "val)" -> "val" *)
    let key = trans_elementaryTypeName left' in
    let value = trans_typeString right' in
    Mapping (key, value)

and trans_array (str : string) : Typ.t =
  let str = S.strip str in
  let left, right = S.rsplit str ~by:"[" in
  (* type[25][30] => (type[25], 30] *)
  let size, _ = S.split right ~by:"]" in
  (* 30] => (30, _) *)
  let t = trans_typeString left in
  let arr_size = try Some (int_of_string size) with _ -> None in
  Array (t, arr_size)

and trans_tuple (str : string) : Typ.t =
  let str = S.chop ~l:6 ~r:1 str in
  (* tuple(uint,,string,) => uint,,string, *)
  if str = "" then TupleType []
  else
    let strs = S.split_on_char ',' str in
    (* uint,,string, => [uint,"",string,""] *)
    TupleType (List.map trans_typeString strs)
;;

let trans_typeDescriptions (json : Y.t) : Typ.t =
  let ts = json |> J.member "typeDescriptions" |> J.member "typeString" |> J.to_string in
  if not (S.starts_with ts "function") then trans_typeString ts
  else
    json |> J.member "typeDescriptions" |> J.member "typeIdentifier" |> J.to_string
    |> trans_typeIdentifier ts
;;

let trans_str_to_typeName = trans_typeString

(* nodeType: X *)
let rec trans_expression (json : Y.t) : exp =
  let typ = trans_typeDescriptions json in
  let node_typ = J.member "nodeType" json in
  let loc = get_loc json in
  let nid = get_id json in
  let einfo = Einfo.{ typ; loc; id = nid } in
  match node_typ with
  | `String "BinaryOperation" -> (
    let left = json |> J.member "leftExpression" |> trans_expression in
    let right = json |> J.member "rightExpression" |> trans_expression in
    match J.member "operator" json with
    | `String "+" -> BinOp (Add, left, right, einfo)
    | `String "-" -> BinOp (Sub, left, right, einfo)
    | `String "*" -> (
      match (left, right) with
      | V (Int n1), V (Frac n2) -> V (Int Q.(to_bigint (of_bigint n1 * n2)))
      | V (Frac n1), V (Int n2) -> V (Int Q.(to_bigint (n1 * of_bigint n2)))
      | V (Frac n1), BinOp (Exponent, V (Int n2), V (Int n3), _) ->
        V (Int Q.(to_bigint (n1 * of_bigint (zpow n2 n3))))
      | _ -> BinOp (Mul, left, right, einfo))
    | `String "/" -> BinOp (Div, left, right, einfo)
    | `String "%" -> BinOp (Mod, left, right, einfo)
    | `String "**" -> BinOp (Exponent, left, right, einfo)
    | `String ">=" -> BinOp (GEq, left, right, einfo)
    | `String ">" -> BinOp (Gt, left, right, einfo)
    | `String "<=" -> BinOp (LEq, left, right, einfo)
    | `String "<" -> BinOp (Lt, left, right, einfo)
    | `String "&&" -> BinOp (LAnd, left, right, einfo)
    | `String "||" -> BinOp (LOr, left, right, einfo)
    | `String "==" -> BinOp (Eq, left, right, einfo)
    | `String "!=" -> BinOp (NEq, left, right, einfo)
    | `String "<<" -> BinOp (ShiftL, left, right, einfo)
    | `String ">>" -> BinOp (ShiftR, left, right, einfo)
    | `String "^" -> BinOp (BXor, left, right, einfo)
    | `String "&" -> BinOp (BAnd, left, right, einfo)
    | `String "|" -> BinOp (BOr, left, right, einfo)
    | _ -> failwith "Unsupported: trans_expression1")
  | `String "Identifier" -> (
    try
      let vname = json |> J.member "name" |> J.to_string in
      let typ = trans_typeDescriptions json in
      let vinfo =
        (* the information is not exact at the moment, but will be updated in the preprocessing. *)
        {
          vloc = loc;
          is_gvar = false;
          vtyp = typ;
          vvis = Private;
          vid = (try json |> J.member "id" |> J.to_int with _ -> assert false);
          refid =
            (try json |> J.member "referencedDeclaration" |> J.to_int with _ -> assert false);
          vscope = -1;
          storage = "";
          flag = false;
          (* should be marked as false *)
          org = Some (Lv (Var (vname, mk_vinfo ~typ ())));
        }
      in
      Lv (Var (vname, vinfo))
    with _ -> assert false)
  | `String "MemberAccess" -> (
    try
      let exp = json |> J.member "expression" |> trans_expression in
      let id = json |> J.member "memberName" |> J.to_string in
      let typ = trans_typeDescriptions json in
      let id_info =
        {
          dummy_vinfo with
          refid = (try json |> J.member "referencedDeclaration" |> J.to_int with _ -> -1);
          vtyp = typ;
          org = Some (Lv (Var (id, mk_vinfo ~typ ())));
        }
      in
      match exp with
      | CallTemp (Lv (Var (fname, _)), args, _, _, _) when fname = "type" && List.length args = 1
        -> (
        let arg = List.hd args in
        let einfo = Einfo.{ loc; typ; id = nid } in
        match arg with
        | Lv (Var (x, _xinfo)) when Typ.is_contract (get_type_exp arg) ->
          TypeInfo (EType (Contract x), id, einfo)
        | ETypeName etyp -> TypeInfo (EType etyp, id, einfo)
        | _ -> raise NotImplemented)
      | _ -> Lv (MemberAccess (exp, id, id_info, typ))
    with _ -> failwith "translator.trans_expression (MemberAccess)")
  | `String "IndexAccess" ->
    let base = json |> J.member "baseExpression" |> trans_expression in
    let idx =
      try json |> J.member "indexExpression" |> trans_expression
      with _ -> failwith "indexExpression may be null: trans_expression"
    in
    Lv (IndexAccess (base, Some idx, typ))
  | `String "Literal" -> (
    match J.member "kind" json with
    | `String "number" -> (
      let factor =
        match J.member "subdenomination" json with
        | `Null -> 1
        | `String "wei" -> 1
        | `String "szabo" -> 1_000_000_000_000
        | `String "finney" -> 1_000_000_000_000_000
        | `String "ether" -> 1_000_000_000_000_000_000
        | `String "seconds" -> 1
        | `String "minutes" -> 60
        | `String "hours" -> 3600
        | `String "days" -> 86400 (* 24 * 3600 *)
        | `String "weeks" -> 604800 (* 7 * 24 * 3600 *)
        | `String "years" -> 31536000 (* 365 * 86400 *)
        | `String "gwei" -> 1_000_000_000
        | _ -> assert false
      in
      let str = json |> J.member "value" |> J.to_string in
      match typ with
      | ConstInt -> (
        match J.member "subdenomination" json with
        | `Null when S.starts_with str "0x" || not (S.contains str 'e' || S.contains str '.') ->
          V (Int (Z.of_string str))
        | _ ->
          let value = Q.of_string str in
          V (Int Q.(to_bigint (value * of_int factor)))
          (* e.g., 0.00001 ether *))
      | EType Address | EType AddressPayable ->
        (* in solv 0.6.12, constant address seems to become 'payable' type *)
        let value = try Z.of_string str with _ -> Z.of_float (float_of_string str) in
        Cast (typ, V (Int (Z.mul value (Z.of_int factor))))
      | ConstReal ->
        let value = Q.of_string str in
        V (Frac Q.(value * of_int factor))
      | _ -> assert false)
    | `String "bool" -> (
      let b = json |> J.member "value" in
      match b with
      | `String "true" -> V (Bool true)
      | `String "false" -> V (Bool false)
      | _ -> assert false)
    | `String "string" ->
      let s =
        try json |> J.member "value" |> J.to_string
        with _ -> json |> J.member "hexValue" |> J.to_string
      in
      Str s
    | `String "hexString" ->
      let s = json |> J.member "hexValue" |> J.to_string in
      Str s
    | `String "unicodeString" -> Str (json |> J.member "value" |> J.to_string)
    | `String s ->
      failwith ("Unsupported: trans_expression2 - " ^ s ^ " : line " ^ string_of_int loc.line)
    | _ -> assert false)
  | `String "FunctionCall" -> (
    match J.member "kind" json with
    | `String "functionCall"
      when json |> J.member "expression" |> J.member "nodeType" = `String "FunctionCallOptions" ->
      let json' = json |> J.member "expression" in
      assert (J.member "nodeType" json' = `String "FunctionCallOptions");
      let args = json |> J.member "arguments" |> trans_functionCallArguments in
      (* should be be json, not json' *)
      let exp = json' |> J.member "expression" |> trans_expression in
      (* for the rest, should be json', not json *)
      let opnames = json' |> J.member "names" |> J.to_list |> List.map J.to_string in
      let ops = json' |> J.member "options" |> J.to_list |> List.map trans_expression in
      assert (List.length opnames = List.length ops);
      assert (List.length opnames <= 2 && List.length ops <= 2);
      let ethop =
        match List.index_of "value" opnames with Some n -> Some (List.at ops n) | None -> None
      in
      let gasop =
        match List.index_of "gas" opnames with Some n -> Some (List.at ops n) | None -> None
      in
      CallTemp (exp, args, ethop, gasop, { loc; typ; id = nid })
    | `String "functionCall" ->
      let exp = json |> J.member "expression" |> trans_expression in
      let args = json |> J.member "arguments" |> trans_functionCallArguments in
      if not (to_string_exp exp = "contract_init") then
        CallTemp (exp, args, None, None, { loc; typ; id = nid })
      else
        let cname = Lv (Var (Typ.get_name_userdef typ, mk_vinfo ~typ ())) in
        CallTemp (exp, cname :: args, None, None, { loc; typ; id = nid })
    | `String "typeConversion" ->
      let arg = json |> J.member "arguments" |> J.to_list in
      assert (List.length arg = 1);
      let exp = trans_expression (List.hd arg) in
      Cast (typ, exp)
    | `String "structConstructorCall" ->
      let exp = json |> J.member "expression" |> trans_expression in
      let args = json |> J.member "arguments" |> trans_functionCallArguments in
      let names = json |> J.member "names" |> J.to_list in
      if List.length names = 0 then (* member names are not given *)
        CallTemp
          (Lv (Var ("struct_init", dummy_vinfo)), exp :: args, None, None, { loc; typ; id = nid })
      else
        let args_names = List.map (fun name -> Lv (Var (J.to_string name, dummy_vinfo))) names in
        CallTemp
          ( Lv (Var ("struct_init2", dummy_vinfo)),
            (exp :: args_names) @ args,
            None,
            None,
            { loc; typ; id = nid } )
    | `String s -> failwith ("Unsupported: trans_expression3-" ^ s)
    | _ -> assert false)
  | `String "UnaryOperation" -> (
    let sub = json |> J.member "subExpression" |> trans_expression in
    match J.member "operator" json with
    | `String "+" -> UnOp (Pos, sub, typ)
    | `String "-" -> UnOp (Neg, sub, typ)
    | `String "!" -> UnOp (LNot, sub, typ)
    | `String "~" -> UnOp (BNot, sub, typ)
    | `String "++" ->
      let pre = json |> J.member "prefix" |> J.to_bool in
      IncTemp (sub, pre, loc)
    | `String "--" ->
      let pre = json |> J.member "prefix" |> J.to_bool in
      DecTemp (sub, pre, loc)
    | _ -> failwith "Unsupported: trans_expression4")
  | `String "TupleExpression" ->
    let tuples = json |> J.member "components" |> J.to_list in
    if Typ.is_array typ then
      Lv (Tuple (List.map (fun e -> try Some (trans_expression e) with _ -> None) tuples, typ))
    else if List.length tuples = 1 then trans_expression (List.hd tuples)
    else Lv (Tuple (List.map (fun e -> try Some (trans_expression e) with _ -> None) tuples, typ))
  | `String "Conditional" ->
    (* cond ? t : f *)
    let cond = json |> J.member "condition" |> trans_expression in
    let t = json |> J.member "trueExpression" |> trans_expression in
    let f = json |> J.member "falseExpression" |> trans_expression in
    CondTemp (cond, t, f, typ, loc)
  | `String "NewExpression" ->
    let ret_typ =
      match typ with
      | FuncType (_, rt) ->
        assert (List.length rt = 1);
        List.hd rt
      | _ -> assert false
    in
    if Typ.is_array ret_typ then Lv (Var ("array_init", mk_vinfo ~typ ()))
    else if Typ.is_contract ret_typ then Lv (Var ("contract_init", mk_vinfo ~typ ()))
    else if Typ.is_struct ret_typ then Lv (Var ("struct_init", mk_vinfo ~typ ()))
    else if Typ.is_enum ret_typ then Lv (Var ("enum_init", mk_vinfo ~typ ()))
    else if Typ.is_dbytes ret_typ then Lv (Var ("dbytes_init", mk_vinfo ~typ ()))
    else if Typ.is_string ret_typ then Lv (Var ("string_init", mk_vinfo ~typ ()))
    else failwith ("NewExpression : " ^ Typ.to_string ret_typ)
  | `String "Assignment" -> (
    let lv = json |> J.member "leftHandSide" |> trans_expression |> exp_to_lv in
    let exp = json |> J.member "rightHandSide" |> trans_expression in
    let typ = json |> J.member "leftHandSide" |> trans_typeDescriptions in
    let op = J.member "operator" json in
    let einfo = Einfo.{ loc; typ; id = nid } in
    match op with
    | `String "=" -> AssignTemp (lv, exp, loc)
    | `String "+=" -> AssignTemp (lv, BinOp (Add, Lv lv, exp, einfo), loc)
    | `String "-=" -> AssignTemp (lv, BinOp (Sub, Lv lv, exp, einfo), loc)
    | `String "*=" -> AssignTemp (lv, BinOp (Mul, Lv lv, exp, einfo), loc)
    | `String "/=" -> AssignTemp (lv, BinOp (Div, Lv lv, exp, einfo), loc)
    | `String "%=" -> AssignTemp (lv, BinOp (Mod, Lv lv, exp, einfo), loc)
    | `String "|=" -> AssignTemp (lv, BinOp (BOr, Lv lv, exp, einfo), loc)
    | `String "^=" -> AssignTemp (lv, BinOp (BXor, Lv lv, exp, einfo), loc)
    | `String "&=" -> AssignTemp (lv, BinOp (BAnd, Lv lv, exp, einfo), loc)
    | `String "<<=" -> AssignTemp (lv, BinOp (ShiftL, Lv lv, exp, einfo), loc)
    | `String ">>=" -> AssignTemp (lv, BinOp (ShiftR, Lv lv, exp, einfo), loc)
    | _ -> failwith " Unsupported: trans_expression5")
  | `String "ElementaryTypeNameExpression" -> (
    (* json AST from solc is slightly differnt per version. *)
    try
      let etyp = json |> J.member "typeName" |> J.to_string |> trans_elementaryTypeName in
      ETypeName etyp
    with _ ->
      let etyp =
        json |> J.member "typeName" |> J.member "name" |> J.to_string |> trans_elementaryTypeName
      in
      ETypeName etyp)
  | `String "IndexRangeAccess" ->
    let base =
      json |> J.member "baseExpression" |> trans_expression |> fun res ->
      match res with Lv lv -> lv | _ -> assert false
    in
    let start =
      try json |> J.member "startExpression" |> trans_expression |> fun res -> Some res
      with _ -> None
    in
    let fin =
      try json |> J.member "endExpression" |> trans_expression |> fun res -> Some res
      with _ -> None
    in
    let einfo = Einfo.{ loc; typ; id = nid } in
    IndexRangeAccess (base, start, fin, einfo)
  | `String s -> failwith ("trans_expression6 - " ^ s ^ " @ " ^ string_of_int loc.line)
  | _ -> failwith "Unsupported: trans_expression7"

(* nodeType: X *)
and trans_functionCallArguments (json : Y.t) : exp list =
  match json with
  | `List l -> List.fold_left (fun acc j -> acc @ [ trans_expression j ]) [] l
  | `Null -> [] (* no arguments: `Null, not `List [] *)
  | _ -> assert false

(* nodeType : O *)
and trans_expressionStatement (json : Y.t) : Stmt.t =
  assert (J.member "nodeType" json = `String "ExpressionStatement");
  let json' = J.member "expression" json in
  let loc = get_loc json' in
  let nid = get_id json' in
  match J.member "nodeType" json' with
  | `String "FunctionCall" -> begin
    match J.member "kind" json' with
    | `String "functionCall"
      when json' |> J.member "expression" |> J.member "nodeType" = `String "FunctionCallOptions" ->
      let json'' = json' |> J.member "expression" in
      assert (J.member "nodeType" json'' = `String "FunctionCallOptions");
      let args = json' |> J.member "arguments" |> trans_functionCallArguments in
      (* should be be json', not json'' *)
      let exp = json'' |> J.member "expression" |> trans_expression in
      (* for the rest, should be json'', not json' *)
      let opnames = json'' |> J.member "names" |> J.to_list |> List.map J.to_string in
      let ops = json'' |> J.member "options" |> J.to_list |> List.map trans_expression in
      assert (List.length opnames = List.length ops);
      assert (List.length opnames <= 2 && List.length ops <= 2);
      let ethop =
        match List.index_of "value" opnames with Some n -> Some (List.at ops n) | None -> None
      in
      let gasop =
        match List.index_of "gas" opnames with Some n -> Some (List.at ops n) | None -> None
      in
      Call (None, exp, args, ethop, gasop, get_loc json')
    | `String "functionCall" ->
      let exp = json' |> J.member "expression" |> trans_expression in
      (* function name *)
      let args = json' |> J.member "arguments" |> trans_functionCallArguments in
      (* Call (None, exp, args) *)
      (* Built-in function checkers. Lists need to be updated. *)
      if is_require exp then begin
        assert (List.length args = 1 || List.length args = 2);
        Assume (List.hd args, loc)
        (* If (List.hd args, Skip, Throw, { if_loc = loc; if_tloc = loc; if_floc = Some loc }) *)
      end
      else if is_assert exp then begin
        assert (List.length args = 1);
        Seq (Assert (List.hd args, "default", get_loc json'), Assume (List.hd args, loc))
        (* If (List.hd args, Skip, Throw, Ifinfo.dummy) *)
      end
      else Call (None, exp, args, None, None, get_loc json')
      (* normal case *)
    | _ -> failwith "Unsupported: trans_expressionStatement1"
  end
  | `String "Assignment" -> (
    let lv = json' |> J.member "leftHandSide" |> trans_expression |> exp_to_lv in
    let exp = json' |> J.member "rightHandSide" |> trans_expression in
    let typ = json' |> J.member "leftHandSide" |> trans_typeDescriptions in
    let op = J.member "operator" json' in
    let einfo = Einfo.{ loc; typ; id = nid } in
    match op with
    | `String "=" -> Assign (lv, exp, loc)
    | `String "+=" -> Assign (lv, BinOp (Add, Lv lv, exp, einfo), loc)
    | `String "-=" -> Assign (lv, BinOp (Sub, Lv lv, exp, einfo), loc)
    | `String "*=" -> Assign (lv, BinOp (Mul, Lv lv, exp, einfo), loc)
    | `String "/=" -> Assign (lv, BinOp (Div, Lv lv, exp, einfo), loc)
    | `String "%=" -> Assign (lv, BinOp (Mod, Lv lv, exp, einfo), loc)
    | `String "|=" -> Assign (lv, BinOp (BOr, Lv lv, exp, einfo), loc)
    | `String "^=" -> Assign (lv, BinOp (BXor, Lv lv, exp, einfo), loc)
    | `String "&=" -> Assign (lv, BinOp (BAnd, Lv lv, exp, einfo), loc)
    | `String "<<=" -> Assign (lv, BinOp (ShiftL, Lv lv, exp, einfo), loc)
    | `String ">>=" -> Assign (lv, BinOp (ShiftR, Lv lv, exp, einfo), loc)
    | _ -> failwith " Unsupported: trans_expressionStatement2")
  | `String "UnaryOperation" -> begin
    let op = J.member "operator" json' in
    match op with
    | `String "++" ->
      let sub = json' |> J.member "subExpression" |> trans_expression in
      let lv = exp_to_lv sub in
      Assign (lv, BinOp (Add, Lv lv, V (Int Z.one), { loc; typ = get_type_lv lv; id = nid }), loc)
    | `String "--" ->
      let sub = json' |> J.member "subExpression" |> trans_expression in
      let lv = exp_to_lv sub in
      Assign (lv, BinOp (Sub, Lv lv, V (Int Z.one), { loc; typ = get_type_lv lv; id = nid }), loc)
    | `String "-" | `String "+" -> Skip (* this does nothing; this is just an expression *)
    | `String "delete" ->
      let sub = json' |> J.member "subExpression" |> trans_expression in
      let lv = Var ("delete", dummy_vinfo) in
      Call (None, Lv lv, [ sub ], None, None, loc)
    | `String s -> failwith ("Unsupported Unary Op: " ^ s)
    | _ -> assert false
  end
  | `String "Identifier" -> Skip
  | `String "BinaryOperation" -> Skip
  | `String "IndexAccess" -> Skip
  | `String "Conditional" ->
    (* cond ? t : f *)
    let cond = json' |> J.member "condition" |> trans_expression in
    (* since json generated by solc does not have proper structure,
     * this additional manipulation step should be forced. *)
    let t =
      `Assoc
        [
          ("expression", J.member "trueExpression" json');
          ("nodeType", `String "ExpressionStatement");
        ]
      |> trans_expressionStatement
    in
    let f =
      `Assoc
        [
          ("expression", J.member "falseExpression" json');
          ("nodeType", `String "ExpressionStatement");
        ]
      |> trans_expressionStatement
    in
    If (cond, t, f, Ifinfo.dummy)
  | `String "TupleExpression" -> Skip
  | `String "FunctionCallOptions" ->
    Skip
    (* e.g., "msg.sender.call{value: msg.value-amountEth};" does nothing. E.g., '("")' should be appended. *)
  | `String s ->
    failwith ("Unsupported: trans_expressionStatement3 - " ^ s ^ " : line " ^ string_of_int loc.line)
  | _ -> assert false
;;

(* nodeType: X *)
let trans_visibility (json : Y.t) : Vis.t =
  match json with
  | `String "public" -> Public
  | `String "internal" -> Internal
  | `String "external" -> External
  | `String "private" -> Private
  | _ -> failwith "trans_visibility"
;;

(* nodeType : O *)
let trans_variable_declaration (json : Y.t) : Var2.t =
  assert (J.member "nodeType" json = `String "VariableDeclaration");
  let vname = json |> J.member "name" |> J.to_string in
  let typ = json |> trans_typeDescriptions in
  (* TODO: function pointers are currently converted to void *)
  let info =
    {
      vloc = json |> get_loc;
      is_gvar = json |> J.member "stateVariable" |> J.to_bool;
      vtyp = typ;
      vvis = json |> J.member "visibility" |> trans_visibility;
      vid = (try json |> J.member "id" |> J.to_int with _ -> assert false);
      refid = (try json |> J.member "id" |> J.to_int with _ -> assert false);
      (* for the declarations, assign themselves. *)
      vscope = (try json |> J.member "scope" |> J.to_int with _ -> assert false);
      storage = (try json |> J.member "storageLocation" |> J.to_string with _ -> assert false);
      flag = true;
      (* true only for variable declarations *)
      org = Some (Lv (Var (vname, mk_vinfo ~typ ())));
    }
  in
  { id = vname; info }
;;

let rec trans_yul_exp (ref : (string * int) list) (ast_id : int) (j : Y.t) : (string * int) list =
  let node_typ = j |> J.member "nodeType" in
  match node_typ with
  | `String "YulIdentifier" ->
    let name = j |> J.member "name" |> J.to_string in
    (* Locals in assembly block do not have references in external blocks. Thus, assign assembly block's AST id. *)
    let refid =
      try List.assoc (j |> J.member "src" |> J.to_string) ref with Not_found -> ast_id
    in
    [ (name, refid) ]
  | `String "YulLiteral" -> []
  | `String "YulFunctionCall" ->
    (* let fname = json |> J.member "functionName" |> J.member "name" |> J.to_string in *)
    let args = j |> J.member "arguments" |> J.to_list in
    let args = List.fold_left (fun acc arg -> acc @ trans_yul_exp ref ast_id arg) [] args in
    args
  | _ -> failwith "trans_yul_exp"
;;

let rec trans_yul_stmt (j : Y.t) (ref : (string * int) list) (ast_id : int) : (string * int) list =
  let node_typ = j |> J.member "nodeType" in
  match node_typ with
  | `String "YulVariableDeclaration" ->
    let lhs = j |> J.member "variables" |> J.to_list in
    let lhs =
      List.map
        (fun j ->
          let name = j |> J.member "name" |> J.to_string in
          (* let _ = print_endline name in
         let _ = print_endline (j |> J.member "src" |> J.to_string) in
         let _ = print_endline (Vocab.string_of_list (fun (src,refid) -> src ^ " : " ^ string_of_int refid) ref) in *)
          (* Locals in assembly block do not have references in external blocks. Thus, assign assembly block's AST id. *)
          let refid =
            try List.assoc (j |> J.member "src" |> J.to_string) ref with Not_found -> ast_id
          in
          (name, refid))
        lhs
    in
    let rhs = j |> J.member "value" |> trans_yul_exp ref ast_id in
    rhs @ lhs
  | `String "YulAssignment" ->
    let lhs = j |> J.member "variableNames" |> J.to_list in
    let lhs =
      List.map
        (fun j ->
          let name = j |> J.member "name" |> J.to_string in
          let refid =
            try List.assoc (j |> J.member "src" |> J.to_string) ref with Not_found -> ast_id
          in
          (name, refid))
        lhs
    in
    let rhs = j |> J.member "value" |> trans_yul_exp ref ast_id in
    rhs @ lhs
  | `String "YulExpressionStatement" -> j |> J.member "expression" |> trans_yul_exp ref ast_id
  | `String "YulForLoop" ->
    let condition = j |> J.member "condition" |> trans_yul_exp ref ast_id in
    let pre = j |> J.member "pre" |> J.member "statements" |> J.to_list in
    let pre = List.fold_left (fun acc j -> acc @ trans_yul_stmt j ref ast_id) [] pre in
    let post = j |> J.member "post" |> J.member "statements" |> J.to_list in
    let post = List.fold_left (fun acc j -> acc @ trans_yul_stmt j ref ast_id) [] post in
    let body = j |> J.member "body" |> J.member "statements" |> J.to_list in
    let body = List.fold_left (fun acc j -> acc @ trans_yul_stmt j ref ast_id) [] body in
    condition @ pre @ post @ body
  | `String "YulIf" ->
    let condition = j |> J.member "condition" |> trans_yul_exp ref ast_id in
    let body = trans_yul_body j ref ast_id in
    condition @ body
  | `String "YulBreak" -> []
  | `String "YulFunctionDefinition" -> trans_yul_body j ref ast_id
  | `String "YulSwitch" ->
    let cases = j |> J.member "cases" |> J.to_list in
    let bodies = List.fold_left (fun acc j -> acc @ trans_yul_body j ref ast_id) [] cases in
    bodies
  | `String s -> failwith ("trans_yul_stmt: " ^ s ^ " @ line " ^ string_of_int (get_loc j).line)
  | _ -> assert false

and trans_yul_body : Yojson.Basic.t -> (string * int) list -> int -> (string * int) list =
 fun json ref ast_id ->
  let body = json |> J.member "body" |> J.member "statements" |> J.to_list in
  let body = List.fold_left (fun acc j -> acc @ trans_yul_stmt j ref ast_id) [] body in
  body
;;

let trans_yul_block (json : Y.t) : (string * int) list =
  assert (J.member "nodeType" json = `String "InlineAssembly");
  let ext_refs = json |> J.member "externalReferences" |> J.to_list in
  let ext_refs =
    List.map
      (fun er -> (er |> J.member "src" |> J.to_string, er |> J.member "declaration" |> J.to_int))
      ext_refs
  in
  let ast_id = json |> J.member "id" |> J.to_int in
  let j = J.member "AST" json in
  assert (J.member "nodeType" j = `String "YulBlock");
  let statements = j |> J.member "statements" |> J.to_list in
  List.fold_left (fun acc j' -> acc @ trans_yul_stmt j' ext_refs ast_id) [] statements
;;

open struct
  module Counter_param = Counter.M ()
end

let param_name = "@Param"
let gen_param_name () = param_name ^ string_of_int (Counter_param.gen ())

(* nodeType: O *)
let trans_parameterList (json : Y.t) : (string * vinfo) list =
  assert (J.member "nodeType" json = `String "ParameterList");
  let parameters = json |> J.member "parameters" |> J.to_list in
  (* 0 parameters => `List [] *)
  let reversed_params =
    List.fold_left
      (fun acc j ->
        let Var2.{ id = vname; info } = trans_variable_declaration j in
        let vname = if S.equal vname "" then gen_param_name () else vname in
        (vname, info) :: acc)
      [] parameters
  in
  let params = List.rev reversed_params in
  params
;;

(* nodeType : X *)
let rec trans_statement (json : Y.t) : Stmt.t =
  let node_typ = J.member "nodeType" json in
  let loc = get_loc json in
  match node_typ with
  | `String "VariableDeclarationStatement" -> (
    (* declaration := initialValue *)
    (* TODO: trim here *)
    let decl = json |> J.member "declarations" |> J.to_list in
    assert (List.length decl > 0);
    let lv =
      if List.length decl = 1 then (* uint a; *)
        let var_decl = List.hd decl in
        let Var2.{ id = vname; info } = trans_variable_declaration var_decl in
        Var (vname, info)
      else (* (a, b, c); *)
        let elst =
          List.map
            (fun v ->
              try
                let Var2.{ id = vname; info } = trans_variable_declaration v in
                Some (Lv (Var (vname, info)))
              with _ -> None)
            decl
        in
        Tuple (elst, Void)
    in
    match J.member "initialValue" json with
    (* TODO: maybe desugared later *)
    | `Null -> Decl lv
    (* TODO: Do we need Decl aside?, e.g., for array length. *)
    | exp -> Assign (lv, trans_expression exp, loc))
  | `String "ExpressionStatement" -> trans_expressionStatement json
  | `String "PlaceholderStatement" -> Placeholder
  | `String "ForStatement" ->
    let init =
      try json |> J.member "initializationExpression" |> trans_statement with _ -> Skip
    in
    (* for ( ;cond;a++) *)
    let cond = try json |> J.member "condition" |> trans_expression with _ -> V (Bool true) in
    (* for (init; ;a++) *)
    let body_wo_last = json |> J.member "body" |> trans_statement in
    let body_last = try json |> J.member "loopExpression" |> trans_statement with _ -> Skip in
    (* for (init;cond; ) *)
    let body = Stmt.Seq (body_wo_last, body_last) in
    Seq (init, While (cond, body))
  | `String "WhileStatement" ->
    let cond = json |> J.member "condition" |> trans_expression in
    let body = json |> J.member "body" |> trans_statement in
    While (cond, body)
  | `String "DoWhileStatement" ->
    let cond = json |> J.member "condition" |> trans_expression in
    let body = json |> J.member "body" |> trans_statement in
    Seq (body, While (cond, body))
  | `String "IfStatement" ->
    let cond = json |> J.member "condition" |> trans_expression in
    let tbody, tloc =
      (json |> J.member "trueBody" |> trans_statement, json |> J.member "trueBody" |> get_loc)
    in
    let fbody, floc =
      match json |> J.member "falseBody" with
      | `Null -> (Stmt.Skip, None)
      | fb -> (trans_statement fb, Some (get_loc fb))
    in
    let ifinfo = Ifinfo.{ if_loc = loc; if_tloc = tloc; if_floc = floc } in
    If (cond, tbody, fbody, ifinfo)
  | `String "Return" -> (
    match J.member "expression" json with
    | `Null -> Return (None, loc)
    | exp -> Return (Some (trans_expression exp), loc))
  | `String "Throw" -> Revert
  | `String "Block" -> trans_block json
  | `String "EmitStatement" -> Skip
  | `String "Break" -> Break
  | `String "Continue" -> Continue
  | `String "InlineAssembly" -> (
    try
      let ext_refs = json |> J.member "externalReferences" |> J.to_list in
      let var_refid_pairs =
        List.map
          (fun j ->
            match j with
            | `Assoc ((s, j') :: []) -> (s, j' |> J.member "declaration" |> J.to_int)
            | _ -> failwith "InlineAssembly")
          ext_refs
      in
      Assembly (var_refid_pairs, loc)
    with _ ->
      let var_refid_pairs = trans_yul_block json in
      Assembly (var_refid_pairs, loc))
  | `String "UncheckedBlock" ->
    let slst = json |> J.member "statements" |> J.to_list |> List.map trans_statement in
    Unchecked (slst, loc)
  | `String "TryStatement" ->
    let json' = json |> J.member "externalCall" in
    assert (J.member "nodeType" json' |> J.to_string = "FunctionCall");
    let extern_call = trans_expression json' in
    assert (match extern_call with CallTemp _ -> true | _ -> false);
    let clauses = json |> J.member "clauses" |> J.to_list in
    let rec f i lst =
      match lst with
      | [] -> Stmt.Revert
      | j :: tl when i = 0 ->
        (* try *)
        assert (j |> J.member "nodeType" |> J.to_string = "TryCatchClause");
        let assign =
          if J.member "parameters" j = `Null then Stmt.Skip (* no parameters *)
          else (* parameter exists *)
            let params = j |> J.member "parameters" |> trans_parameterList in
            let lv = params_to_lv params in
            Assign (lv, extern_call, loc)
        in
        let stmt = trans_block (j |> J.member "block") in
        If (Lv (gen_tmpvar (EType Bool)), Seq (assign, stmt), f (i + 1) tl, Ifinfo.dummy)
      | j :: tl ->
        (* catch *)
        let stmt = trans_block (j |> J.member "block") in
        If (Lv (gen_tmpvar (EType Bool)), stmt, f (i + 1) tl, Ifinfo.dummy)
    in
    f 0 clauses
  | `String "RevertStatement" -> Revert
  | `String s ->
    failwith ("Unsupported: trans_statement - " ^ s ^ " : line " ^ string_of_int loc.line)
  | _ -> assert false

(* nodeType : O *)
and trans_block (json : Y.t) : Stmt.t =
  assert (J.member "nodeType" json = `String "Block");
  let statements = json |> J.member "statements" |> J.to_list in
  List.fold_left
    (fun acc j ->
      let new_stmt = trans_statement j in
      Stmt.Seq (acc, new_stmt))
    Skip statements
;;

(** usual: defined modifiers appear as invocations

    unusual: consturctor invocations appear as modifiers *)
let is_usual_modifier (cnames : string list) (json : Y.t) : bool =
  assert (J.member "nodeType" json = `String "ModifierInvocation");
  let modname = json |> J.member "modifierName" |> J.member "name" |> J.to_string in
  not (List.mem modname cnames)
;;

(* nodeType: O *)
let trans_modifierInvocation (json : Y.t) : Mod_call.t =
  assert (J.member "nodeType" json = `String "ModifierInvocation");
  let name = json |> J.member "modifierName" |> J.member "name" |> J.to_string in
  let args = json |> J.member "arguments" |> trans_functionCallArguments in
  let loc = get_loc json in
  { id = name; args; loc }
;;

(* generate Constructor call *)
let trans_inheritanceSpecifier (json : Y.t) : Mod_call.t =
  assert (J.member "nodeType" json = `String "InheritanceSpecifier");
  let name = json |> J.member "baseName" |> J.member "name" |> J.to_string in
  let args = json |> J.member "arguments" |> trans_functionCallArguments in
  let loc = get_loc json in
  { id = name; args; loc }
;;

let resolve_cnstr_calls : Mod_call.t list -> Mod_call.t list -> Mod_call.t list =
 fun inherit_calls mod_calls ->
  (* In solc 0.4x, mod_calls has the priority over the inherit_calls. *)
  (* In recent solc, specifying arguments for both places is an error. *)
  (* E.g.,
   * Inherit: A(5) B(3), Mod: A(8) C(7) => B(3) A(8) C(7) *)
  let combined = inherit_calls @ mod_calls in
  let combined = List.rev combined in
  (* rev list to give the priority to mod_calls *)
  List.fold_left
    (fun acc (m : Mod_call.t) ->
      let b = List.exists (fun (m' : Mod_call.t) -> m'.id = m.id) acc in
      if b then acc else m :: acc)
    [] combined
;;

let trans_isConstructor (j : Y.t) : bool =
  assert (J.member "nodeType" j = `String "FunctionDefinition");
  try j |> J.member "isConstructor" |> J.to_bool (* solc 0.4x *)
  with _ -> (
    (* solc 0.5x *)
    match J.member "kind" j with
    | `String "constructor" -> true
    | `String "function" -> false
    | `String "fallback" -> false
    | `String "receive" -> false
    | `String s -> failwith ("trans_isConstructor: " ^ s)
    | _ -> assert false)
;;

let trans_fname (j : Y.t) (is_constructor : bool) (cid : string) : string =
  assert (J.member "nodeType" j = `String "FunctionDefinition");
  try
    match J.member "kind" j with
    (* solc 0.5x *)
    | `String "constructor" -> cid
    | `String "function" -> j |> J.member "name" |> J.to_string
    | `String "fallback" -> "@fallback"
    | `String "receive" -> "@receive" (* solc 0.6x *)
    | `String s -> failwith ("trans_fname : " ^ s)
    | _ -> assert false
  with _ ->
    (* solc 0.4x *)
    let fname = j |> J.member "name" |> J.to_string in
    let fname = if is_constructor then cid else if fname = "" then "@fallback" else fname in
    fname
;;

let trans_payable (j : Y.t) : bool =
  assert (J.member "nodeType" j = `String "FunctionDefinition");
  try j |> J.member "payable" |> J.to_bool (* 0.4x *)
  with _ -> (
    (* 0.5x *)
    match J.member "stateMutability" j with
    | `String "payable" -> true
    | `String "nonpayable" -> false
    | `String "view" -> false
    | `String "pure" -> false
    | _ -> failwith "stateMutability")
;;

let trans_mutability (j : Y.t) : State_mut.t =
  assert (J.member "nodeType" j = `String "FunctionDefinition");
  match J.member "stateMutability" j with
  | `String "payable" -> Payable
  | `String "nonpayable" -> NonPayable
  | `String "view" -> View
  | `String "pure" -> Pure
  | _ -> failwith "stateMutability"
;;

let trans_structDefinition (j : Y.t) : Struct.t =
  let name = j |> J.member "canonicalName" |> J.to_string in
  let fields = List.map trans_variable_declaration (j |> J.member "members" |> J.to_list) in
  { name; fields }
;;

(* nodeType : O *)
let trans_contractDefinition : string list -> Struct.t list -> Yojson.Basic.t -> Contract.t =
 fun cnames global_structs json ->
  let cid = json |> J.member "name" |> J.to_string in
  let contract_parts = json |> J.member "nodes" |> J.to_list in
  let cinfo =
    Cinfo.
      {
        numid = json |> J.member "id" |> J.to_int;
        inherit_order = List.map J.to_int (json |> J.member "linearizedBaseContracts" |> J.to_list);
        lib_typ_list = [];
        ckind = json |> J.member "contractKind" |> J.to_string |> Ckind.of_string;
      }
  in
  let base_contracts = json |> J.member "baseContracts" |> J.to_list in
  (* A is B,C,D => base_contracts : [B; C; D] *)
  let cnstr_calls_inherit =
    List.fold_left
      (fun acc base ->
        let cnstr_call = trans_inheritanceSpecifier base in
        if List.length cnstr_call.args > 0 then acc @ [ cnstr_call ]
          (* constructors are called starting from parents *)
        else acc)
      [] base_contracts
  in
  (* NOTE: lists are stored in a reversed order *)
  let cid, gvar_decls, structs, enums, func_defs, cinfo =
    List.fold_left
      (fun ((cid, gvar_decls, structs, enums, func_defs, cinfo) as acc) j ->
        let node_typ = J.member "nodeType" j in
        match node_typ with
        | `String "VariableDeclaration" ->
          let Var2.{ id = vname; info } = trans_variable_declaration j in
          let expop =
            match j |> J.member "value" with `Null -> None | exp -> Some (trans_expression exp)
          in
          let decl : State_var_decl.t = { name = vname; assign = expop; vinfo = info } in
          (cid, decl :: gvar_decls, structs, enums, func_defs, cinfo)
        | `String "EventDefinition" ->
          (* Event is modeled as a function with internal visibility and a skip body *)
          let fname = j |> J.member "name" |> J.to_string in
          let params = j |> J.member "parameters" |> trans_parameterList in
          let finfo : Finfo.t =
            {
              is_constructor = false;
              is_payable = false;
              is_modifier = false;
              mod_list = [];
              mod_list2 = [];
              param_loc = Loc.dummy;
              ret_param_loc = Loc.dummy;
              fvis = Internal;
              mutability = Pure;
              (* event can be considered as pure *)
              fid = j |> J.member "id" |> J.to_int;
              floc = get_loc j;
              blk_loc = Loc.dummy;
              scope = cinfo.Cinfo.numid;
              scope_s = cid;
              (* to be filled by preprocessor *)
              org_scope_s = cid;
              cfg = Cfg.empty;
            }
          in
          let stmt : Stmt.t = Skip in
          let func : Func.t =
            {
              name = fname;
              params;
              ret_params = [];
              body = stmt;
              extcall_locations = [];
              info = finfo;
            }
          in
          (cid, gvar_decls, structs, enums, func :: func_defs, cinfo)
        | `String "EnumDefinition" ->
          let name = j |> J.member "name" |> J.to_string in
          let members =
            List.map
              (fun j' -> j' |> J.member "name" |> J.to_string)
              (j |> J.member "members" |> J.to_list)
          in
          let enum : Enums.t = { id = name; members } in
          (cid, gvar_decls, structs, enums @ [ enum ], func_defs, cinfo)
        | `String "StructDefinition" ->
          let structure = trans_structDefinition j in
          (cid, gvar_decls, structs @ [ structure ], enums, func_defs, cinfo)
        | `String "UsingForDirective" ->
          let lib_name = j |> J.member "libraryName" |> J.member "name" |> J.to_string in
          (* let typ = trans_typeDescriptions j in *)
          let typ = j |> J.member "typeName" in
          if typ = `Null then
            let cinfo =
              Cinfo.{ cinfo with lib_typ_list = (lib_name, Glob) :: cinfo.lib_typ_list }
            in
            (cid, gvar_decls, structs, enums, func_defs, cinfo)
          else
            let typ = trans_typeDescriptions typ in
            let cinfo =
              Cinfo.{ cinfo with lib_typ_list = (lib_name, Typ typ) :: cinfo.lib_typ_list }
            in
            (cid, gvar_decls, structs, enums, func_defs, cinfo)
        | `String "FunctionDefinition" ->
          let is_constructor = trans_isConstructor j in
          let fname = trans_fname j is_constructor cid in
          (* let fname = if is_constructor then "@constructor" else trans_fname j is_constructor cid in *)
          let params = j |> J.member "parameters" |> trans_parameterList in
          let ret_params = j |> J.member "returnParameters" |> trans_parameterList in
          let body =
            if j |> J.member "implemented" |> J.to_bool then j |> J.member "body" |> trans_block
            else Skip (* function without definitions *)
          in
          let modifiers = j |> J.member "modifiers" |> J.to_list in
          (* executed in the order of usual mod call => constructor mod call *)
          let mod_calls =
            List.fold_left
              (fun acc j' ->
                if is_usual_modifier cnames j' then acc @ [ trans_modifierInvocation j' ] else acc)
              [] modifiers
          in
          let cnstr_calls_mod =
            List.fold_left
              (fun acc j' ->
                if not (is_usual_modifier cnames j') then acc @ [ trans_modifierInvocation j' ]
                else acc)
              [] modifiers
          in
          let cnstr_calls = resolve_cnstr_calls cnstr_calls_inherit cnstr_calls_mod in
          let info : Finfo.t =
            {
              is_constructor;
              is_payable = trans_payable j;
              is_modifier = false;
              mod_list = mod_calls;
              mod_list2 = (if is_constructor then cnstr_calls else []);
              param_loc = j |> J.member "parameters" |> get_loc;
              ret_param_loc = j |> J.member "returnParameters" |> get_loc;
              fvis = j |> J.member "visibility" |> trans_visibility;
              mutability = trans_mutability j;
              fid = j |> J.member "id" |> J.to_int;
              floc = get_loc j;
              blk_loc = (try j |> J.member "body" |> get_loc with _ -> Loc.dummy);
              scope = cinfo.numid;
              scope_s = cid;
              org_scope_s = cid;
              cfg = Cfg.empty;
            }
          in
          let func =
            { Func.name = fname; params; ret_params; body; extcall_locations = []; info }
          in
          (cid, gvar_decls, structs, enums, func :: func_defs, cinfo)
        | `String "ModifierDefinition" ->
          let fname = j |> J.member "name" |> J.to_string in
          let params = j |> J.member "parameters" |> trans_parameterList in
          let finfo : Finfo.t =
            {
              is_constructor = false;
              is_payable = false;
              is_modifier = true;
              mod_list = [];
              (* no modifier invocations in modifier definition *)
              mod_list2 = [];
              (* same as above *)
              param_loc = Loc.dummy;
              ret_param_loc = Loc.dummy;
              fvis = j |> J.member "visibility" |> trans_visibility;
              mutability = NonPayable;
              (* field does not exist *)
              fid = j |> J.member "id" |> J.to_int;
              floc = get_loc j;
              blk_loc = (try j |> J.member "body" |> get_loc with _ -> Loc.dummy);
              scope = cinfo.numid;
              scope_s = cid;
              org_scope_s = cid;
              cfg = Cfg.empty;
            }
          in
          let stmt = j |> J.member "body" |> trans_block in
          let func = Func.mk ~name:fname ~params ~ret_params:[] ~body:stmt ~info:finfo in
          (cid, gvar_decls, structs, enums, func :: func_defs, cinfo)
        | `String "ErrorDefinition" -> acc
        | `String s -> failwith ("Unsupported: trans_contractDefinition - " ^ s)
        | _ -> assert false)
      (cid, [], global_structs, [], [], cinfo)
      contract_parts
  in
  let decls, funcs = (List.rev gvar_decls, List.rev func_defs) in
  let b = List.exists Func.is_constructor func_defs in
  if b then { name = cid; decls; structs; enums; funcs; info = cinfo }
  else
    (* make a new constructor if does not exist *)
    let fname = cid in
    let params = [] in
    let cnstr_calls = resolve_cnstr_calls cnstr_calls_inherit [] in
    let finfo : Finfo.t =
      {
        is_constructor = true;
        is_payable = false;
        is_modifier = false;
        mod_list = [];
        mod_list2 = cnstr_calls;
        param_loc = Loc.dummy;
        ret_param_loc = Loc.dummy;
        fvis = Public;
        fid = -1;
        mutability = NonPayable;
        floc = Loc.dummy;
        blk_loc = Loc.dummy;
        scope = cinfo.numid;
        scope_s = cid;
        org_scope_s = cid;
        cfg = Cfg.empty;
      }
    in
    let cnstr = Func.mk ~name:fname ~params ~ret_params:[] ~body:Skip ~info:finfo in
    { name = cid; decls = gvar_decls; structs; enums; funcs = cnstr :: func_defs; info = cinfo }
;;

let run (json : Y.t) (lines : string list) : Pgm.t =
  let () = record_end_of_lines lines in
  assert (J.member "nodeType" json = `String "SourceUnit");
  let l = json |> J.member "nodes" |> J.to_list in
  (* 0 nodes => `List [] *)
  let global_structs =
    List.filter (fun j -> J.member "nodeType" j = `String "StructDefinition") l
  in
  let global_structs = List.map trans_structDefinition global_structs in
  let contracts = List.filter (fun j -> J.member "nodeType" j = `String "ContractDefinition") l in
  let cnames = List.map (fun j -> j |> J.member "name" |> J.to_string) contracts in
  List.map (trans_contractDefinition cnames global_structs) contracts
;;
