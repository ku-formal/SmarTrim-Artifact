open! Vocab
open Vocab.Batteries
open Frontend
open Frontend.Lang

type t = { alloc : (Var.t, Z.t) Map.t; bit_usage : int } [@@deriving show { with_path = false }]

let bit_usage h = h.bit_usage

let mk (pgm : Pgm.t) =
  let c = Pgm.main pgm in
  let vars = var_contract c in
  let vars = vars |> Set.to_list |> List.filter (fun v -> Typ.struct_score (snd v) >= 0) in
  let max_score = vars |> List.map (T2._2 %> Typ.struct_score) |> List.fold_left Int.max 0 in
  let alloc = vars |> List.mapi (fun i v -> (v, Z.of_int i)) |> List.to_seq |> Map.of_seq in
  { alloc; bit_usage = (max_score + 1) * 256 }
;;

let tid_offset = Z.(of_int 2 ** 64)

let split_org_tid s =
  try
    let origin, r = String.split ~by:"@T" s in
    let n = Int.of_string r in
    Some (origin, n)
  with Not_found | Failure _ -> None
;;

let get_offset (handler : t) (v : Var.t) =
  let first_component =
    match split_org_tid (fst v) with
    | Some (origin, n) ->
      let origin = Var.mk origin (snd v) in
      let z =
        try Map.find origin handler.alloc with Not_found -> Pp.failwithf "Not_found: %s" (fst v)
      in
      Z.(z + (tid_offset * of_int (Int.add n 1)))
    | None ->
      let z =
        try Map.find v handler.alloc with Not_found -> Pp.failwithf "Not_found: %s" (fst v)
      in
      z
  in
  Z.(first_component * (of_int 2 ** handler.bit_usage))
;;
