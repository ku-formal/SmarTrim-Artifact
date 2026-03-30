(* export modules to outside *)
module Install = Install
module Ver = Ver
(* export modules to outside end *)

open! Vocab

exception CompilationError of Ver.t list
exception UnsupportedSolc of Ver.t

let version = Ver.mk

(** {b Example}.
    [get_solc_path (Ver.mk 0 4 22) = "./.solc-select/artifacts/solc-0.4.22/solc-0.4.22"] *)
let get_solc_path (v : Ver.t) =
  let solc_name = "solc-" ^ Ver.to_string v in
  Os.cat_fromhome [ ".solc-select"; "artifacts"; solc_name; solc_name ]
;;

let get_solv_cands (solv : Ver.t option) filepath =
  let mk = Ver.mk in
  let src = In_channel.(with_open_text filepath input_all) in
  let candidates_from_src = Ver.candidates_from_src src in
  match solv with
  | None -> candidates_from_src
  | Some solv ->
    if solv < mk 0 4 0 then raise @@ UnsupportedSolc solv
    else if solv < Ver.least_supported then [ mk 0 4 25 ]
    else if solv = mk 0 5 0 then [ mk 0 5 1; mk 0 5 17 ]
      (* solc_0.5.0 --ast-compact-json produces a solc error. *)
    else if List.mem solv Ver.versions then
      let correct_candids =
        List.filter Ver.(fun v -> solv.major = v.major && solv.minor = v.minor) Ver.representatives
      in
      solv :: correct_candids
    else raise @@ UnsupportedSolc solv
;;

let get_json_ast silent filepath (v : Ver.t) =
  if not silent then print_endline (Pp.spf "[INFO] compiling with solc ver. %a..." Ver.pp_human v);
  let buf =
    Unix.open_process_in (Pp.spf "%s --ast-compact-json %s 2>/dev/null" (get_solc_path v) filepath)
  in
  try
    let () =
      ignore (input_line buf);
      ignore (input_line buf);
      ignore (input_line buf);
      ignore (input_line buf)
    in
    let json = Yojson.Basic.from_channel buf in
    match Unix.close_process_in buf with
    | WEXITED 0 ->
      let () =
        if not silent then print_endline (Pp.spf "[INFO] compiled with solc ver. %a" Ver.pp_human v)
      in
      Ok (v, json)
    | _ -> Error ()
  with _ ->
    let _ = Unix.close_process_in buf in
    Error ()
;;

let get_json_ast ?(silent = false) (solv : Ver.t option) filepath =
  let cands = get_solv_cands solv filepath in
  let r =
    List.fold_left
      (fun ret v -> match ret with Ok _ -> ret | Error _ -> get_json_ast silent filepath v)
      (Error ()) cands
  in
  match r with Ok r -> r | Error () -> raise @@ CompilationError cands
;;

let is_compilable (solv : Ver.t) file : bool =
  try
    let _ = get_json_ast ~silent:true (Some solv) file in
    true
  with CompilationError _ -> false
;;
