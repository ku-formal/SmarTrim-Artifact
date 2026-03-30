type t = { signed : bool; size : int; data : Z.t }

let normalize bv =
  let mask = Z.sub (Z.shift_left Z.one bv.size) Z.one in
  let masked_data = Z.logand bv.data mask in
  if bv.signed && Z.testbit bv.data (bv.size - 1) then
    (* if signed negative, the masked data represents the value of (actual value + 2^b) *)
    let value_range = Z.shift_left Z.one bv.size in
    let unsigned_val = masked_data in
    let signed_val = Z.sub unsigned_val value_range in
    { bv with data = signed_val }
  else { bv with data = masked_data }
;;

let mk ~signed ~size value =
  let bv = { signed; size; data = value } in
  normalize bv
;;

let add bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for addition"
  else
    let result = Z.add bv1.data bv2.data in
    normalize { bv1 with data = result }
;;

let sub bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for subtraction"
  else
    let result = Z.sub bv1.data bv2.data in
    normalize { bv1 with data = result }
;;

let mul bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for multiplication"
  else
    let result = Z.mul bv1.data bv2.data in
    normalize { bv1 with data = result }
;;

let div bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for division"
  else if Z.equal bv2.data Z.zero then failwith "Division by zero"
  else
    let result = if bv1.signed then Z.ediv bv1.data bv2.data else Z.div bv1.data bv2.data in
    normalize { bv1 with data = result }
;;

let rem bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for remainder"
  else if Z.equal bv2.data Z.zero then failwith "Division by zero"
  else
    let result = if bv1.signed then Z.erem bv1.data bv2.data else Z.rem bv1.data bv2.data in
    normalize { bv1 with data = result }
;;

let logand bv1 bv2 =
  if bv1.size <> bv2.size then failwith "Bitvectors must have the same size for bitwise AND"
  else
    let result = Z.logand bv1.data bv2.data in
    { bv1 with data = result; signed = bv1.signed && bv2.signed }
;;

let logor bv1 bv2 =
  if bv1.size <> bv2.size then failwith "Bitvectors must have the same size for bitwise OR"
  else
    let result = Z.logor bv1.data bv2.data in
    { bv1 with data = result; signed = bv1.signed && bv2.signed }
;;

let logxor bv1 bv2 =
  if bv1.size <> bv2.size then failwith "Bitvectors must have the same size for bitwise XOR"
  else
    let result = Z.logxor bv1.data bv2.data in
    { bv1 with data = result; signed = bv1.signed && bv2.signed }
;;

let lognot bv =
  let mask = Z.sub (Z.shift_left Z.one bv.size) Z.one in
  let result = Z.logxor bv.data mask in
  { bv with data = result }
;;

let shift_left bv n =
  if n < 0 then failwith "Shift amount must be non-negative"
  else
    let result = Z.shift_left bv.data n in
    normalize { bv with data = result }
;;

let shift_right bv n =
  if n < 0 then failwith "Shift amount must be non-negative"
  else
    let result = if bv.signed then Z.shift_right bv.data n else Z.shift_right_trunc bv.data n in
    normalize { bv with data = result }
;;

let resize bv new_size =
  if new_size <= 0 then failwith "Size must be positive" else normalize { bv with size = new_size }
;;

let change_signedness new_signed bv =
  if bv.signed = new_signed then bv
  else
    let normalized = normalize bv in
    { normalized with signed = new_signed }
;;

let to_int bv = Z.to_int bv.data
let of_int ~signed ~size n = mk ~signed ~size (Z.of_int n)

let to_string bv =
  let value_str = Z.to_string bv.data in
  let sign_str = if bv.signed then "s" else "u" in
  Printf.sprintf "%s<%d>(%s)" sign_str bv.size value_str
;;

let to_hex_string bv =
  let hex_str = Z.format "x" bv.data in
  let sign_str = if bv.signed then "s" else "u" in
  Printf.sprintf "%s<%d>(0x%s)" sign_str bv.size hex_str
;;

let compare bv1 bv2 =
  if bv1.size <> bv2.size || bv1.signed <> bv2.signed then
    failwith "Bitvectors must have the same size and signedness for comparison"
  else Z.compare bv1.data bv2.data
;;

let cmp = compare
let equal bv1 bv2 = compare bv1 bv2 = 0
let eq = equal
let lt bv1 bv2 = compare bv1 bv2 < 0
let leq bv1 bv2 = compare bv1 bv2 <= 0
let gt bv1 bv2 = compare bv1 bv2 > 0
let geq bv1 bv2 = compare bv1 bv2 >= 0
