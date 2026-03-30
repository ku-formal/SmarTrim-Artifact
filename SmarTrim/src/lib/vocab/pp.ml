(** Pretty printer combinator. Similar with ocaml Fmt module, but reimplemented in my flavor.

    Written since 2025. *)

open struct
  module Set = Batteries.Set
  module Map = Batteries.Map
  module F = Format
end

(** printer type. *)
type 'a t = F.formatter -> 'a -> unit

(** {b Usage Example:} [pf ppf "%i" n] prints [n] to the pretty-printer [ppf].

    {b Usage Example of %a:} [pf ppf "%a" intlist_printer intlist] prints [intlist] to the
    pretty-printer [ppf], using the integer list printer [list_printer : int list t]. *)
let pf = F.fprintf

(** = {!Format.printf} *)
let pr = F.printf

(** = {!Format.eprintf} *)
let epr = F.eprintf

(** = {!Format.asprintf} *)
let spf = F.asprintf

(** = {!Format.kasprintf} *)
let kspf = F.kasprintf

(** {b Usage Example:} [str n] returns the string of [n].

    {b Usage Example of %a:} [str intlist_printer intlist] prints [intlist] to the pretty-printer
    [ppf], using the integer list printer [list_printer : int list t]. *)
let str : 'a t -> 'a -> string = fun fmt elem -> spf "%a" fmt elem

let stdout = F.std_formatter
let stderr = F.err_formatter

let oc oc =
  let fmt = F.formatter_of_out_channel oc in
  fmt
;;

let bool : bool t = F.pp_print_bool
let int : int t = F.pp_print_int
let float : float t = F.pp_print_float
let z : Z.t t = Z.pp_print
let q : Q.t t = Q.pp_print
let char : char t = F.pp_print_char
let string : string t = F.pp_print_string
let lit (s : string) : 'a t = fun ppf _ -> string ppf s
let any fmt : 'a t = fun ppf _ -> pf ppf fmt
let cut : 'a t = fun ppf _ -> F.pp_print_cut ppf ()
let space : 'a t = fun ppf _ -> F.pp_print_space ppf ()
let space_nocut : 'a t = fun ppf -> lit " " ppf
let break i j : 'a t = fun ppf _ -> F.pp_print_break ppf i j
let comma : 'a t = fun ppf -> any ",@ " ppf
let comma_nocut : 'a t = fun ppf -> any "," ppf
let semi : 'a t = fun ppf -> any ";@ " ppf
let colon : 'a t = fun ppf -> any ":@ " ppf
let flush : 'a t = fun ppf _ -> F.pp_print_flush ppf ()
let nop : 'a t = fun _fmt _ppf -> ()
let using (f : 'a -> 'b) (pp : 'b t) : 'a t = fun ppf v -> pp ppf (f v)
let const (pp_v : 'a t) (v : 'a) : 'b t = fun ppf _ -> pp_v ppf v
let seq ?(sep : unit t = F.pp_print_cut) : 'a t -> 'a list t = F.pp_print_list ~pp_sep:sep
let parens (pp_v : 'a t) : 'a t = fun ppf -> pf ppf "@,(@,%a@,)@," pp_v
let brackets (pp_v : 'a t) : 'a t = fun ppf -> pf ppf "@,[@,%a@,]@," pp_v
let braces (pp_v : 'a t) : 'a t = fun ppf -> pf ppf "@,{@,%a@,}@," pp_v

(** Similar to [wrap], but no break hints are automatically added *)
let surround s1 s2 (pp : 'a t) : 'a t =
 fun ppf value ->
  string ppf s1;
  pp ppf value;
  string ppf s2
;;

(** [wrap l r pp_v] returns a printer that prints *)
let wrap l r (pp_v : 'a t) : 'a t =
 fun ppf v ->
  string ppf l;
  cut ppf ();
  pp_v ppf v;
  cut ppf ();
  string ppf r
;;

let wrap_hv ?(indent = 2) l r (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_hvbox ppf indent;
  string ppf l;
  cut ppf ();
  pp ppf value;
  break 0 (-indent) ppf ();
  string ppf r;
  F.pp_close_box ppf ()
;;

let parens_hv ?(indent = 2) = wrap_hv ~indent "(" ")"
let brackets_hv ?(indent = 2) = wrap_hv ~indent "[" "]"
let braces_hv ?(indent = 2) = wrap_hv ~indent "{" "}"

let box ?(indent = 0) (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_box ppf indent;
  pp ppf value;
  F.pp_close_box ppf ()
;;

let hbox (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_hbox ppf ();
  pp ppf value;
  F.pp_close_box ppf ()
;;

let vbox ?(indent : int = 0) (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_vbox ppf indent;
  pp ppf value;
  F.pp_close_box ppf ()
;;

let hvbox ?(indent : int = 0) (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_hvbox ppf indent;
  pp ppf value;
  F.pp_close_box ppf ()
;;

let hovbox ?(n : int = 0) (pp : 'a t) : 'a t =
 fun ppf value ->
  F.pp_open_hovbox ppf n;
  pp ppf value;
  F.pp_close_box ppf ()
;;

let append (pp_v0 : 'a t) (pp_v1 : 'a t) : 'a t =
 fun ppf (v : 'a) ->
  pp_v0 ppf v;
  pp_v1 ppf v
;;

let ( ++ ) : 'a t -> 'a t -> 'a t = append

let rec concat ?(sep : unit t = cut) (pps : 'a t list) : 'a t =
 fun ppf value ->
  match pps with
  | [] -> ()
  | [ pp ] -> pp ppf value
  | pp :: tl ->
    pp ppf value;
    sep ppf ();
    concat ~sep tl ppf value
;;

let list (ppi : 'a t) : 'a list t =
  let ctt : 'a list t = seq ~sep:semi ppi in
  brackets_hv ctt
;;

(** Transforms a pretty printer to a pretty list printer, but attaches an index for every elements.
    Note that no break hints are added between index and content. *)
let listi ?(offset = 0) (ppi : 'a t) : 'a list t =
 fun ppf l ->
  let l = List.mapi (fun i v -> (i + offset, v)) l in
  let ppi ppf (i, v) = pf ppf "%i:%a" i ppi v in
  let ctt = seq ~sep:semi ppi in
  brackets_hv ctt ppf l
;;

(** Assoc list. Default [kvsep] is [":@,"]. *)
let assoc ?(kvsep : unit t = lit ":" ++ cut) (ppk : 'k t) (ppv : 'v t) : ('k * 'v) list t =
  let pair ppf (k, v) = pf ppf "%a%a%a" ppk k kvsep () ppv v in
  let ctt : ('k * 'v) list t = seq ~sep:semi pair in
  braces_hv ctt
;;

let set (ppi : 'a t) : 'a Set.t t =
  let ctt : 'a Set.t t = using Set.to_list (seq ~sep:semi ppi) in
  braces_hv ctt
;;

(** Map printer. Default [kvsep] is [":@,"]. *)
let map ?(kvsep : unit t = lit ":" ++ cut) (ppk : 'k t) (ppv : 'v t) : ('k, 'v) Map.t t =
 fun ppf value ->
  let assoc_list = Map.to_seq value |> List.of_seq in
  assoc ~kvsep ppk ppv ppf assoc_list
;;

let option_ignore_none (ppi : 'a t) : 'a option t =
 fun ppf (v : 'a option) -> match v with Some v -> pf ppf "%a" ppi v | None -> ()
;;

let option (ppi : 'a t) : 'a option t =
 fun ppf (v : 'a option) ->
  match v with Some v -> concat [ lit "Some"; space; parens_hv ppi ] ppf v | None -> pf ppf "None"
;;

let pair ?(wrap = wrap "(" ")") ?(sep = comma) ppe1 ppe2 =
  let aux ppf (a, b) =
    ppe1 ppf a;
    sep ppf ();
    ppe2 ppf b
  in
  wrap aux
;;

let tuple2 = pair
let pairn pp = pair pp pp
let tuple2n = pairn
let tuple3 epp1 epp2 epp3 ppf (a, b, c) = pf ppf "(@,%a,@ %a,@ %a@,)" epp1 a epp2 b epp3 c
let tuple3n pp = tuple3 pp pp pp

(* Utils for error messages *)

let failwithf fmt = kspf failwith fmt
let invalid_argf fmt = kspf invalid_arg fmt
let requiredf e fmt = kspf (Misc.required e) fmt
let exn_invalid_argf fmt = kspf (fun s -> Invalid_argument s) fmt
let result_err fmt = kspf Result.error fmt

(* An example code to practice Format module. Cannot refer anywhere outside. *)
open! struct
  module ExampleJSON = struct
    type[@warning "-37"] t =
      | Null
      | Boolean of bool
      | Number of float
      | String of string
      | Array of t list
      | Object of (string * t) list

    let number_to_string (n : float) =
      let s = F.asprintf "%.15g" n in
      if Float.of_string s = n then s else F.asprintf "%.17g" n
    ;;

    let pp_string_body ppf =
      String.iter (function
        | '"' -> pf ppf {|\"|} (* {|"|} *)
        | '\\' -> pf ppf {|\\|}
        | '\b' -> pf ppf {|\b|}
        | '\x0C' -> pf ppf {|\f|}
        | '\n' -> pf ppf {|\n|}
        | '\r' -> pf ppf {|\r|}
        | '\t' -> pf ppf {|\t|}
        | '\x00' .. '\x1F' as non_print_char -> pf ppf {|\u%.4X|} (Char.code non_print_char)
        | char -> pf ppf {|%c|} char)
    ;;

    let box pp ppf value = pf ppf "@[<hv>%a@]" pp value

    let rec pp ppf = function
      | Null -> pf ppf "null"
      | Boolean b -> pf ppf "%b" b
      | Number n -> pf ppf "%s" (number_to_string n)
      | String s -> pf ppf {|"%a"|} pp_string_body s
      | Array a -> pf ppf "[@;<0 2>%a@;<0 0>]" (seq ~sep:comma (box pp)) a
      | Object o -> pf ppf "{@;<0 2>%a@;<0 0>}" (seq ~sep:comma (box pp_pair)) o

    and pp_pair ppf (field, value) = pf ppf {|"%a": %a|} pp_string_body field pp value

    let[@warning "-32"] to_string = str (box pp)
  end
end
