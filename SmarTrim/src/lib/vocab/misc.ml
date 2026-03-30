(** identity *)
let id x = x

let triple_fst (a, _, _) = a
let triple_snd (_, b, _) = b
let triple_third (_, _, c) = c

(** fill zeros in front of number-representing strings *)
let zfill num str =
  if num > String.length str then BatString.repeat "0" (num - String.length str) ^ str else str
;;

let replace_str : string -> string -> string -> string =
 fun str sub rep ->
  let b, res = BatString.replace ~str ~sub ~by:rep in
  let _ = assert b in
  res
;;

exception NotImplemented

(************************************)
(* Utilities from BatPervasives     *)
(************************************)

(** The undefined function.

    Evaluating [undefined x] always fails and raises an exception "Undefined". Optional argument
    [message] permits the customization of the error message.*)
let undefined = BatPervasives.undefined

(** Function composition: the mathematical [o] operator. [f % g] is [fun x -> f (g x)]. It is
    similar to Haskell's [.].

    Examples: the following are equivalent: [f (g (h x))], [f @@ g @@ h x], [f % g % h @@ x]. *)
let ( % ) = BatPervasives.( % )

(** Piping function composition. [f %> g] is [fun x -> g (f x)]. Whereas [f % g] applies [g] first
    and [f] second, [f %> g] applies [f], then [g]. Note that it plays well with pipes, so for
    instance [x |> f %> g %> h |> i %> j] yields the expected result... but in such cases it's still
    recommended to use [|>] only. Note that it replaces pre-2.0 [|-], which {i didn't} integrate
    with pipes. *)
let ( %> ) = BatPervasives.( %> )

(** Like {!BatOption.default}, with the arguments reversed. [None |? 10] returns [10], while
    [Some "foo" |? "bar"] returns ["foo"].

    {b Note} This operator does not short circuit like [( || )] and [( && )]. Both arguments will be
    evaluated. *)
let ( |? ) = BatPervasives.( |? )

(** [cmp_with_key ~cmp f l r] is [cmp (f l) (f r)]. Default [cmp] is [Stdlib.compare]. *)
let cmp_with_key ?(cmp = Stdlib.compare) f l r = cmp (f l) (f r)

let eq_with_key ?(eq = ( = )) f l r = eq (f l) (f r)

include BatEnum.Infix

(** Ignore its second argument.

    [const x] is the function which always returns [x].*)
let const x _ = x

(** Allows application of a function in the middle of a pipe sequence without disturbing the
    sequence. [x |> tap f] evaluates to [x], but has the side effect of [f x]. Useful for debugging.
*)
let tap f x =
  f x;
  x
;;

(************************************)
(* Ref Utilities                    *)
(************************************)

let ( += ) ref n = ref := !ref + n
let ( -= ) ref n = ref := !ref - n
let ( *= ) ref n = ref := !ref * n
let ( /= ) ref n = ref := !ref / n
let ( +.= ) ref n = ref := !ref +. n
let ( -.= ) ref n = ref := !ref -. n
let ( *.= ) ref n = ref := !ref *. n
let ( /.= ) ref n = ref := !ref /. n

(************************************)
(* Set/Map Utilities                *)
(************************************)

module ListMap = struct
  (** ListMap module, for BatMaps whose values are lists. *)

  open Batteries

  type ('k, 'v) t = ('k, 'v list) Map.t

  let empty : ('k, 'v) t = Map.empty

  let add k v (m : ('k, 'v) t) : ('k, 'v) t =
    match Map.find_opt k m with None -> Map.add k [ v ] m | Some l -> Map.add k (v :: l) m
  ;;

  (** [find x m] returns the current binding of [x] in [m], or returns [[]] if no such binding
      exists. *)
  let find k (m : ('k, 'v) t) : 'v list = try Map.find k m with Not_found -> []
end

(************************************)
(* Print Utilities                  *)
(************************************)

let print_list ?(first = "[") ?(last = "]") ?(sep = "; ") print_e oc (l : 'a list) =
  let open Out_channel in
  match l with
  | [] ->
    output_string oc first;
    output_string oc last
  | [ e ] ->
    output_string oc first;
    print_e oc e;
    output_string oc last
  | h :: t ->
    output_string oc first;
    print_e oc h;
    List.iter
      (fun e ->
        output_string oc sep;
        print_e oc e)
      t;
    output_string oc last
;;

let print_enum ?(first = "[") ?(last = "]") ?(sep = "; ") print_e oc e =
  print_list ~first ~last ~sep print_e oc (BatList.of_enum e)
;;

let print_set ?(first = "{") ?(last = "}") ?(sep = "; ") print_e oc s =
  let l = BatSet.to_list s in
  print_list ~first ~last ~sep print_e oc l
;;

let print_assoc_list ?(first = "{") ?(last = "}") ?(sep = "; ") ?(kvsep = ": ") print_k print_v oc
    al =
  let open Out_channel in
  let print_pair print_k print_v oc (k, v) =
    let () = print_k oc k in
    output_string oc kvsep;
    let () = print_v oc v in
    ()
  in
  print_list ~first ~last ~sep (print_pair print_k print_v) oc al
;;

let print_map ?(first = "{") ?(last = "}") ?(sep = "; ") ?(kvsep = ": ") print_k print_v oc s =
  let l = BatMap.to_seq s in
  let l = List.of_seq l in
  print_assoc_list ~first ~last ~sep ~kvsep print_k print_v oc l
;;

(************************************)
(* IO Utilities                     *)
(************************************)

let in_channel_of_string string =
  let in_file_descr, out_file_descr = Unix.pipe () in
  let in_channel = Unix.in_channel_of_descr in_file_descr in
  let out_channel = Unix.out_channel_of_descr out_file_descr in
  output_string out_channel string;
  close_out out_channel;
  in_channel
;;

(************************************)
(* Misc                             *)
(************************************)

let get_datetime_string time =
  let time = Unix.localtime time in
  Format.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (1900 + time.tm_year) (time.tm_mon + 1)
    time.tm_mday time.tm_hour time.tm_min time.tm_sec
;;

let invalid_arg_here (here : Lexing.position) msg =
  invalid_arg @@ Printf.sprintf "%s:%i: %s" here.pos_fname here.pos_lnum msg
;;

let get_memory_usage_ps () =
  let ic = Unix.open_process_in "ps -o rss= -p $$" in
  try
    let line = input_line ic in
    close_in ic;
    Some (int_of_string (String.trim line))
    (* in KB *)
  with _ ->
    close_in ic;
    None
;;

(** [random_bits size s] returns a random big_int of bitwidth [size] or less. *)
let random_bits size (s : BatRandom.State.t) : BatBig_int.t =
  let open BatBig_int in
  let mask = sub_big_int (shift_left_big_int one size) one in
  let rec bits acc =
    if gt_big_int acc mask then and_big_int acc mask
    else
      let tail = BatRandom.State.bits s in
      let tail = of_int tail in
      let acc = or_big_int (shift_left_big_int acc 30) tail in
      bits acc
  in
  bits zero
;;

let random_string size (s : BatRandom.State.t) : string =
  let rec f acc =
    if List.length acc >= size then acc
    else
      let ascii_a = int_of_char 'a' in
      let random_offset = Random.State.int s 26 in
      let c = char_of_int (ascii_a + random_offset) in
      f @@ (c :: acc)
  in
  BatString.of_list @@ f []
;;

let required b errmsg = if b then () else failwith errmsg
