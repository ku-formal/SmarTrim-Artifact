(** Use [open Vocab.Batteries] to set batteries modules as default.

    Some modules like {!BatPrintf}, {!BatIO} should be excluded from this list, as they lack
    compatibility to other libraries. We use {!Printf} as default. *)

open Batteries
module Deque = Deque
module DynArray = DynArray
module Enum = Enum

module Gc = struct
  include Gc

  open struct
    type stat = Gc.stat = {
      minor_words : float;
      promoted_words : float;
      major_words : float;
      minor_collections : int;
      major_collections : int;
      heap_words : int;
      heap_chunks : int;
      live_words : int;
      live_blocks : int;
      free_words : int;
      free_blocks : int;
      largest_free : int;
      fragments : int;
      compactions : int;
      top_heap_words : int;
      stack_size : int;
      forced_major_collections : int;
    }
    [@@deriving show { with_path = false }]

    type control = Gc.control = {
      minor_heap_size : int;
      major_heap_increment : int;
      space_overhead : int;
      verbose : int;
      max_overhead : int;
      stack_limit : int;
      allocation_policy : int;
      window_size : int;
      custom_major_ratio : int;
      custom_minor_ratio : int;
      custom_minor_max_size : int;
    }
    [@@deriving show { with_path = false }]
  end

  let pp_stat = pp_stat
  let show_stat = show_stat
  let pp_control = pp_control
  let show_control = show_control
end

module Int = struct
  include Int

  let pp = Pp.int

  let show_compact ?(round : [ `Down | `Up | `Off ] = `Down) n =
    ignore round;
    (* todo *)
    if n < 1_000 then to_string n
    else if n < 1_000_000 then
      let n' = n / 1_000 in
      to_string n' ^ "k"
    else if n < 1_000_000_000 then
      let n' = n / 1_000_000 in
      to_string n' ^ "m"
    else
      let n' = n / 1_000_000_000 in
      to_string n' ^ "t"
  ;;

  let pp_compact ?(round = `Down) ppf n = Pp.pf ppf "%s" (show_compact ~round n)
end

module List = struct
  include List

  let print = Misc.print_list
  let pp = Pp.list

  (** [fold_combi f acc l] is same as
      [List.fold_left f acc [(l0, l1); (l0, l2); ...; (l1, l2); ...]], but does not require O(|l|^2)
      memory space. *)
  let fold_combi (f : 'acc -> 'a * 'a -> 'acc) (acc : 'acc) (l : 'a list) : 'acc =
    let rec aux (acc : 'acc) (l : 'a list) =
      match l with
      | [] -> acc
      | hd :: tl ->
        let g acc a = f acc (hd, a) in
        let acc = List.fold_left g acc tl in
        aux acc tl
    in
    aux acc l
  ;;

  (** [for_all2_cartesian f l1 l2] checks whether [forall e1 in l1. forall e2 in l2. f e1 e2] is
      true. *)
  let for_all2_product f l1 l2 =
    l1 |> List.for_all (fun e1 -> l2 |> List.for_all (fun e2 -> f e1 e2))
  ;;

  let common_prefix ?(equal = ( = )) l1 l2 =
    let rec aux acc l1 l2 =
      match (l1, l2) with
      | h1 :: t1, h2 :: t2 -> if equal h1 h2 then aux (h1 :: acc) t1 t2 else (acc, l1, l2)
      | _ -> (acc, l1, l2)
    in
    let acc, l1, l2 = aux [] l1 l2 in
    (rev acc, l1, l2)
  ;;

  let takedrop_while p l =
    let rec aux acc l =
      match l with [] -> (acc, []) | h :: t -> if p h then aux (h :: acc) t else (acc, l)
    in
    let l1, l2 = aux [] l in
    (rev l1, l2)
  ;;

  let take2 l = match l with a :: b :: _ -> Some (a, b) | _ -> None
  let take3 l = match l with a :: b :: c :: _ -> Some (a, b, c) | _ -> None
  let take4 l = match l with a :: b :: c :: d :: _ -> Some (a, b, c, d) | _ -> None

  (** Simple utilities for association lists. *)
  module A = struct
    type ('key, 'value) t = ('key * 'value) list

    let keys (l : ('key, 'value) t) = List.map fst l
    let values (l : ('key, 'value) t) = List.map snd l
    let get_exn = assoc
    let get = assoc_opt
    let get_inv_exn = assoc_inv
    let remove = remove_assoc
    let mem = mem_assoc
    let modify_exn = modify
    let modify_def_exn = modify_def
    let modify = modify_opt
    let set_exn key value list = modify_exn key (const value) list
    let set key value list = modify_def_exn value key (const value) list

    let compare cmp_key cmp_value (l : ('key, 'value) t) (r : ('key, 'value) t) =
      let comparator = Tuples.T2.compare ~cmp1:cmp_key ~cmp2:cmp_value in
      compare comparator l r
    ;;

    let pp (pp_k : 'key Pp.t) (pp_v : 'value Pp.t) : ('key, 'value) t Pp.t =
      Pp.(list (tuple2 pp_k pp_v))
    ;;
  end
end

module Map = struct
  include Map

  let to_list m = m |> to_seq |> List.of_seq
  let print = Misc.print_map
  let pp = Pp.map

  module Make (Ord : OrderedType) = struct
    include Make (Ord)

    let pp pp_k pp_v ppf (mapping : 'a t) =
      let l = to_list mapping in
      Pp.assoc pp_k pp_v ppf l
    ;;
  end
end

module Option = Option
module Random = Random

module Result = struct
  include Result

  let required b errmsg = if b then Ok () else Error errmsg
end

module Set = struct
  include Set

  let print = Misc.print_set
  let pp = Pp.set

  module Make (Ord : OrderedType) = struct
    include Make (Ord)

    let pp pp_v ppf (set : t) =
      let l = to_list set in
      Pp.braces (Pp.seq ~sep:(Pp.lit "; ") pp_v) ppf l
    ;;
  end
end

module String = String
