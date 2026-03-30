open! Vocab

(** example: 0.4.22 -> [{ major = 0; minor = 4; patch = 22; }] *)
type t = { major : int; minor : int; patch : int } [@@deriving show]

open struct
  let ( *~~ ) lbd ubd = List.init (ubd - lbd + 1) (( + ) lbd)
end

let mk major minor patch : t = { major; minor; patch }

(** "x.y.z" to version type. *)
let of_string s : t option =
  let l = String.split_on_char '.' s in
  let l = List.map int_of_string_opt l in
  match l with [ Some i; Some j; Some k ] -> Some { major = i; minor = j; patch = k } | _ -> None
;;

let of_string_exn s : t =
  match of_string s with
  | Some v -> v
  | None -> Pp.invalid_argf "Solc.Ver.of_string_exn : %s: not in the format of i.j.k" s
;;

(** human-readable version name. *)
let pp_human ppf v = Pp.pf ppf "%i.%i.%i" v.major v.minor v.patch

(** human-readable to-string. *)
let to_string v = Printf.sprintf "%i.%i.%i" v.major v.minor v.patch

let get_major_minor solv = (solv.major, solv.minor)

let versions =
  let mk major minor patches = List.map (fun p -> mk major minor p) patches in
  mk 0 4 (16 *~~ 26)
  (* 0.4.0~~11 do not support --ast-compact-json, 12~15 do not support stateMutability *)
  @ mk 0 5 (1 *~~ 17) (* there was a report that 0.5.0 --ast-compact-json produces an error *)
  @ mk 0 6 (0 *~~ 12)
  @ mk 0 7 (0 *~~ 6)
  @ mk 0 8 (0 *~~ 29)
;;

let least_supported = List.hd versions
let representatives = [ mk 0 4 16; mk 0 4 26; mk 0 5 17; mk 0 6 12; mk 0 7 6; mk 0 8 28 ]

open struct
  let representatives_first (l : t list) : t list =
    let rep, nonrep = List.partition (flip List.mem representatives) l in
    rep @ nonrep
  ;;

  let extract_pragma_version_info (pragma_solidity : string) : (string * t) list =
    let open Re in
    let re = compile Dfa.pragma_version_pat in
    let found = all re pragma_solidity in
    List.map
      (fun group ->
        let operator, major, minor, patch = T4.mapn (Group.get group) (1, 2, 3, 4) in
        let major, minor, patch = T3.mapn int_of_string (major, minor, patch) in
        (operator, { major; minor; patch }))
      found
  ;;

  let extract_pragma_solidity_info (src : string) : (string * t) list =
    let open Re in
    let src = Dfa.kill_comment src in
    let re = compile Dfa.pragma_solidity_pat in
    let found = all re src in
    let infos = List.map (flip Group.get 0) found in
    let ret = List.map extract_pragma_version_info infos in
    List.concat ret
  ;;

  let filter_version ((operator, v) : string * t) (l : t list) : t list =
    let filterer v' =
      match operator with
      | "=" | "" -> v' = v
      | "<=" -> v' <= v
      | ">=" -> v' >= v
      | "<" -> v' < v
      | ">" -> v' > v
      | "^" -> get_major_minor v' = get_major_minor v
      | _ ->
        Pp.invalid_argf "Solc.Ver : Wrong operator : %s. Note: This is VeriSmart's logic error."
          operator
    in
    List.filter filterer l
  ;;
end

let candidates_from_src (src : string) : t list =
  let filterers : (string * t) list = extract_pragma_solidity_info src in
  let filterers : (t list -> t list) list = List.map filter_version filterers in
  let l = List.fold_left (fun acc f -> f acc) versions filterers in
  representatives_first l
;;
