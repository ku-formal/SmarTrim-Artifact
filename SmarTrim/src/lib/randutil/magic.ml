let get_magic_address (i : int) =
  let salt = "54ddba3e62d0ee9481000773870211cf1ce9b0b6c630bfec5304704de04ab3cc" in
  let s = salt ^ Int.to_string i in
  let t = Digestif.KECCAK_256.digest_string s in
  let s = Vocab.Pp.str Digestif.KECCAK_256.pp t in
  let s = String.sub s 0 40 in
  Crypto.checksummed_exn ("0x" ^ s)
;;
