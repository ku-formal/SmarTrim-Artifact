open Lang

(** "Contract Name.Struct Name" or "Struct Name" (for global structs) *)
type t = (string, Var2.t list) Map.t

let add = Map.add
let empty = Map.empty

let mk_smap_c : t -> Contract.t -> t =
 fun smap c -> List.fold_left (fun acc (s : Struct.t) -> add s.name s.fields acc) smap c.structs
;;

let mk_smap contracts = List.fold_left mk_smap_c empty contracts

let find sname smap =
  try Map.find sname smap with Not_found -> Pp.failwithf "StructMap.find : %s" sname
;;

let to_string (smap : t) : string =
  "{" ^ "\n"
  ^ Map.foldi
      (fun x ylst acc ->
        acc ^ x ^ " -> " ^ string_of_list ~first:"[" ~last:"]" ~sep:"," Var2.to_string ylst ^ "\n")
      smap ""
  ^ "}"
;;

let all_member_vars (smap : t) =
  Map.bindings smap |> List.map T2._2 |> List.concat |> List.map Var2.to_var
;;
