(** Paths.

    {b History} Before 2025, the name of this module was Path. Now we use this name, as the previous
    name raises a name collision with a part of the OCaml compiler when used with [dune utop]. *)

open! Frontend
open Frontend.Lang
open! Vocab
open Vocab.Batteries

module Impl : sig
  type t = private { id : int; fkey : Fkey.t; basic_path : Node.t list }

  val mk : Fkey.t -> Node.t list -> t
  val shuffle_id : Global.t -> t list -> t list
end = struct
  type t = { id : int; fkey : Fkey.t; basic_path : Node.t list }

  module Counter_paths = Counter.M ()

  let mk fkey basic_path =
    let id = Counter_paths.gen () in
    { id; fkey; basic_path }
  ;;

  let compare_inside l r = T2.compare (l.fkey, l.basic_path) (r.fkey, r.basic_path)

  let shuffle_id (global : Global.t) (paths : t list) : t list =
    let paths = List.sort_uniq compare_inside paths in
    let state = Random.State.make global.seed in
    let n = List.length paths in
    let ids = List.init n (( + ) 1) in
    let ids = List.shuffle ~state ids in
    List.map2 (fun (p : t) n -> { p with id = n }) paths ids
  ;;
end

type t = Impl.t = private { id : int; fkey : Fkey.t; basic_path : Node.t list }
[@@deriving fields ~getters]

let compare (l : t) (r : t) : int =
  match Int.compare l.id r.id with
  | 0 -> Stdlib.compare (l.fkey, l.basic_path) (r.fkey, r.basic_path)
  | n -> n
;;

(** This function uses thread safe counter *)
let mk = Impl.mk

(** Eliminates redundancies and shuffles path ids. *)
let shuffle_id = Impl.shuffle_id

let to_string p = Fkey.to_string p.fkey ^ " : " ^ to_string_path p.basic_path
let to_fname (p : t) : string = p.fkey.func

(* if len(lst) = 2, returns empty list *)
let get_mid = function [] | [ _ ] -> [] | _ :: tl -> List.remove_at (List.length tl - 1) tl

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
