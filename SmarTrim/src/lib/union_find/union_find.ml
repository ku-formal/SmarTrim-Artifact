(** Union_find Data Structure. *)

open Batteries

(** A UnionFind data structure. *)
type 'k t = ('k, 'k) Hashtbl.t

(** Makes an empty union-find DS.
    @param n : initial guess of requiring elements. *)
let create n : 'k t = Hashtbl.create n

let rec find_parent tbl key =
  if not (Hashtbl.mem tbl key) then
    let () = Hashtbl.add tbl key key in
    key
  else
    let par = Hashtbl.find tbl key in
    if key = par then par
    else
      let p = find_parent tbl par in
      let () = Hashtbl.replace tbl key p in
      p
;;

let union tbl key1 key2 =
  let px = find_parent tbl key1 in
  let py = find_parent tbl key2 in
  if px <> py then Hashtbl.replace tbl px py
;;

let union_by_enum tbl (e : ('a * 'a) Enum.t) =
  e |> Enum.iter (fun (key1, key2) -> union tbl key1 key2)
;;

let union_by_list tbl (list : ('a * 'a) List.t) =
  list |> List.iter (fun (key1, key2) -> union tbl key1 key2)
;;

let is_joined tbl key1 key2 =
  let px = find_parent tbl key1 in
  let py = find_parent tbl key2 in
  px = py
;;

let to_string string_of_t (tbl : 'k t) : string =
  let x (a, b) = Printf.sprintf "(%s -> %s);" (string_of_t a) (string_of_t b) in
  let join l r = l ^ " " ^ r in
  let content = tbl |> Hashtbl.to_list |> List.map x |> List.reduce join in
  "[" ^ content ^ "]"
;;
