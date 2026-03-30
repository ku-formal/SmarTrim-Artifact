module DFA1 = struct
  type t = { state : int; stack : char list }

  let new_dfa : t = { state = 0; stack = [] }

  let next (a : t) c : t =
    let stack = a.stack in
    let stack = match a.state with 0 | 1 | 5 | 6 -> c :: stack | _ -> stack in
    let state, stack =
      match (a.state, c) with
      | 0, '/' -> (1, stack)
      | 0, '"' -> (5, stack)
      | 0, _ -> (0, stack)
      | 1, '/' -> (2, stack |> List.tl |> List.tl)
      | 1, '*' -> (3, stack |> List.tl |> List.tl)
      | 1, _ -> (0, stack)
      | 2, '\n' -> (0, '\n' :: stack)
      | 2, _ -> (2, stack)
      | 3, '*' -> (4, stack)
      | 3, _ -> (3, stack)
      | 4, '/' -> (0, stack)
      | 4, '*' -> (4, stack)
      | 4, _ -> (3, stack)
      | 5, '"' -> (0, stack)
      | 5, '\\' -> (6, stack)
      | 5, _ -> (5, stack)
      | 6, _ -> (5, stack)
      | _ -> failwith ""
    in
    { state; stack }
  ;;
end

let kill_comment (src : string) : string =
  let open DFA1 in
  let dfa = BatString.fold_left next new_dfa src in
  dfa.stack |> BatList.rev |> BatString.of_list
;;

let kill_keyword ?(by = "") (kwd : string) (src : string) : string =
  let open Re in
  let pattern = word (str kwd) in
  let re = compile pattern in
  replace_string re ~by src
;;

let pragma_version_pat =
  let open Re in
  seq
    [
      group @@ alt [ str "^"; str "<="; str ">="; str "="; str ">"; str "<"; epsilon ];
      rep space;
      group @@ rep1 digit;
      rep space;
      char '.';
      rep space;
      group @@ rep1 digit;
      rep space;
      char '.';
      rep space;
      group @@ rep1 digit;
      rep space;
    ]
;;

let pragma_solidity_pat =
  let open Re in
  seq [ str "pragma"; rep1 space; str "solidity"; rep space; rep1 pragma_version_pat; char ';' ]
;;

let selfdestruct_pat =
  let open Re in
  seq
    [
      str "selfdestruct";
      rep space;
      char '(';
      group (non_greedy (rep any));
      char ')';
      rep space;
      char ';';
    ]
;;

let is_pragma_version (s : string) =
  let open Re in
  let re = compile pragma_version_pat in
  execp re s
;;

let is_pragma_solidity (s : string) =
  let open Re in
  let re = compile pragma_solidity_pat in
  execp re s
;;

let kill_pragma_solidity (s : string) : string =
  let open Re in
  let re = compile pragma_solidity_pat in
  replace_string re ~by:"" s
;;

let kill_selfdestruct ?(by = "") (s : string) : string =
  let open Re in
  let re = compile selfdestruct_pat in
  replace_string re ~by s
;;
