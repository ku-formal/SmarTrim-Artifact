(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(* Vocabularies *)

let flip f = fun y x -> f x y

(** This applies {!List.fold_left}, but the argument type is similar with {!BatSet.fold}. *)
let list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b =
 fun f list init -> List.fold_left (flip f) init list
;;

let list_fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c =
 fun f list1 list2 init ->
  let f' acc a b = f a b acc in
  List.fold_left2 f' init list1 list2
;;

let link_by_sep sep s acc = if acc = "" then s else acc ^ sep ^ s

let string_of_list ?(first = "[") ?(last = "]") ?(sep = ";") : ('a -> string) -> 'a list -> string =
 fun string_of_v list ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ list_fold add_string_of_v list "" ^ last
;;

let string_of_set ?(first = "{") ?(last = "}") ?(sep = ",") :
    ('a -> string) -> 'a BatSet.t -> string =
 fun string_of_v set ->
  let add_string_of_v v acc = link_by_sep sep (string_of_v v) acc in
  first ^ BatSet.fold add_string_of_v set "" ^ last
;;

let string_of_map ?(first = "{") ?(last = "}") ?(sep = ",\n") ?(indent = "") :
    ('a -> string) -> ('b -> string) -> ('a, 'b) BatMap.t -> string =
 fun string_of_k string_of_v map ->
  let add_string_of_k_v k v acc =
    let str = string_of_k k ^ " -> " ^ string_of_v v in
    link_by_sep (sep ^ indent) str acc
  in
  if BatMap.is_empty map then "empty"
  else indent ^ first ^ BatMap.foldi add_string_of_k_v map "" ^ last
;;

(* fixpoint operator for set *)
let rec fix : ('a BatSet.t -> 'a BatSet.t) -> 'a BatSet.t -> 'a BatSet.t =
 fun f init ->
  let next = f init in
  if BatSet.subset next init then init else fix f next
;;

(****************************************)
(*** End of Implementation from ROPAS ***)
(****************************************)
