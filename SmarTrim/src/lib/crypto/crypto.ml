open! Vocab
open Vocab.Batteries

open struct
  let is_valid_address (address_string : string) : bool =
    let address_length = 40 in
    if String.starts_with address_string "0x" then
      let address_string = String.lchop ~n:2 address_string in
      if String.length address_string = address_length then
        let address_string = String.lowercase_ascii address_string in
        let test c = match c with '0' .. '9' | 'a' .. 'f' -> true | _ -> false in
        String.for_all test address_string
      else false
    else false
  ;;
end

(** Checksums. [checksummed_exn address_string] returns the expression of checksummed address. This
    implements {!https://github.com/ethereum/ercs/blob/master/ERCS/erc-55.md}.

    @raise Invalid_arg when the given string is not a valid address form *)
let checksummed_exn (address_string : string) : string =
  if not @@ is_valid_address address_string then
    Pp.invalid_argf "Invalid string : %s" address_string;
  let address_string = String.lchop ~n:2 address_string in
  let address_string = String.lowercase_ascii address_string in
  let hashed_address = Digestif.KECCAK_256.digest_string address_string in
  let hashed_address : string = Pp.str Digestif.KECCAK_256.pp hashed_address in
  let folder list i c =
    match c with
    | '0' .. '9' -> c :: list
    | 'a' .. 'f' ->
      let hashed_address_nibble = hashed_address.[i] in
      let new_c =
        match hashed_address_nibble with '0' .. '7' -> c | _ -> Char.uppercase_ascii c
      in
      new_c :: list
    | _ -> Pp.invalid_argf "unexpected charactor : %c" c
  in
  let l = List.fold_lefti folder [] (String.to_list address_string) in
  let l = List.rev l in
  (* note: accumulated in the reversed order *)
  let ret = String.of_list l in
  "0x" ^ ret
;;

let checksummed s = try Some (checksummed_exn s) with Invalid_argument _ -> None
