open! Checker
open! Frontend
open Frontend.Lang
open Options
open Semantics
open! Vocab

let iter = ref 0

module QMap = struct
  module BatMap = BatMap.Make (Query.ID)

  type t = v BatMap.t
  and k = Query.ID.t
  and v = Query.Status.t * Fkey.t BatSet.t * iter
  and iter = int
          
  let mk_key = Query.get_qid
  
  let add = BatMap.add
  let find = BatMap.find
  let empty = BatMap.empty
  let bindings = BatMap.bindings
  let for_all = BatMap.for_all
  let map = BatMap.mapi
end

let get_proven : (QMap.k * QMap.v) list -> (QMap.k * QMap.v) list
= fun lst -> List.filter (fun (_,(stat,_,_)) -> stat = Query.Status.Proven) lst

let get_unproven : (QMap.k * QMap.v) list -> (QMap.k * QMap.v) list
= fun lst -> List.filter (fun (_,(stat,_,_)) -> stat = Query.Status.Unproven) lst

let filter ~(kind : Kind.t) lst = 
  List.filter (fun ((qid : Query.ID.t), _) -> Kind.equal kind qid.kind) lst
  
let exclude ~(kind : Kind.t) lst = 
  List.filter (fun ((qid : Query.ID.t), _) -> not @@ Kind.equal kind qid.kind) lst

let print_result_per_query lst =
  List.iteri (fun i ((qid : Query.ID.t), (stat, _fkey, _iter)) ->
    let s1 = "[" ^ string_of_int (i+1) ^ "]" ^ " " ^ "[" ^ Kind.show qid.kind ^ "]" ^ " " in
    let s2 = Query.ID.show_for_stdout qid ^ " : " ^ Query.Status.show stat in
    print_endline (s1 ^ s2);
  ) lst

let print : QMap.t -> QMap.t -> unit
= fun qmap reg_qmap ->
  let lst = QMap.bindings qmap in
  let reg_lst = QMap.bindings reg_qmap in
  let unproven_lst = get_unproven lst in
  (* let proven_lst = get_proven lst in *)
  print_endline "\n===== Vulnerability Report =====";
  print_result_per_query lst;

  if !Chk.reg then
    (print_endline "";
     print_endline "===== Regression Report =====";
     if List.length reg_lst = 0 then print_endline "- Regression report is empty"
     else print_result_per_query reg_lst);

  print_endline "";
  print_endline "============ Statistics ============";
  print_endline ("# Iter                    : " ^ string_of_int !iter);
  print_endline ("# Alarm / Query           : " ^ string_of_int (List.length unproven_lst) ^ " / " ^ string_of_int (List.length lst));
  if !Chk.io      then print_endline ("- integer over/underflow  : " ^ string_of_int (List.length (filter ~kind:IO unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:IO lst)));
  if !Chk.dz      then print_endline ("- division-by-zero        : " ^ string_of_int (List.length (filter ~kind:DZ unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:DZ lst)));
  if !Chk.assert_ then print_endline ("- assertion violation     : " ^ string_of_int (List.length (filter ~kind:ASSERT unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:ASSERT lst)));
  if !Chk.su      then print_endline ("- suicidal             : " ^ string_of_int (List.length (filter ~kind:SU unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:SU lst)));
  if !Chk.el      then print_endline ("- ether-leaking           : " ^ string_of_int (List.length (filter ~kind:EL unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:EL lst)));
  if !Chk.re      then print_endline ("- reentrancy-leaking      : " ^ string_of_int (List.length (filter ~kind:RE_EL unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:RE_EL lst)));
  if !Chk.re      then print_endline ("- reentrancy              : " ^ string_of_int (List.length (filter ~kind:RE unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:RE lst)));
  if !Chk.tx      then print_endline ("- tx.origin               : " ^ string_of_int (List.length (filter ~kind:TX_ORG unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:TX_ORG lst)))

let proved_nontrivially : (QMap.k * QMap.v) list -> int
= fun lst ->
  let lst' = List.filter (fun (_,(stat,_,i)) -> stat = Query.Status.Proven && i > 1) lst in
  List.length lst'

let mk_json_lst qmap =
  List.mapi (fun i ((qid : Query.ID.t), (stat, fkeys, _iter)) ->
    `Assoc [("no", `String (string_of_int (i+1)));
            ("kind", `String (Kind.show qid.kind));
            ("line", `Int qid.loc.line);
            ("signatures", `List (fkeys
                                  |> BatSet.to_list
                                  |> List.map (fun Fkey.{ contract = c; func = f; param_typs = typs } ->
                                      `Assoc [("contractName", `String c);
                                              ("methodName", `String f);
                                              ("argTypes", `List (typs |> List.map (fun t -> `String (Typ.to_string t))))])));
                                              ("exp", `String qid.exp);
                                              ("status", `String (Query.Status.show stat))]
  ) (QMap.bindings qmap)

let csv_header = ["no"; "kind"; "signature"; "line"; "exp"]

let qlst_to_csv global lst =
  List.mapi (fun i ((qid : Query.ID.t),(_stat,_,_iter)) ->
    let no = string_of_int (i+1) in
    let kind = Kind.show qid.kind in
    (* cve-2018-14004, line 85 / 2018-13519, line 178 *)
    let fkey = try to_string_fkey2 (Func.fkey (Global.find_func_containing_line qid.loc.line global)) with Not_found -> "none" in
    let exp = qid.exp in
    [no; kind; fkey; string_of_int qid.loc.line; exp]
  ) lst

(* To avoid misguiding repair procedure, as state-chaning related alarms are more than RE-EL alarms. *)
(* e.g., 0x01f8c4e3fa3edeb29e514cba738d87ce8c091d3f *)
let adjust_re_alarms all unproven =
  let re_el_qs = filter ~kind:RE_EL all in
  let re_el_unproven = filter ~kind:RE_EL unproven in
  let re_qs = filter ~kind:RE all in
  let re_unproven = filter ~kind:RE unproven in

  if List.length re_qs > 0 && List.length re_unproven = 0 then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE (* re-safe *)

  else if List.length re_el_qs > 0 && List.length re_el_unproven = 0 then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE (* re-safe *)

  else if List.length re_el_qs > 0 && List.length re_el_unproven > 0 then
    unproven |> exclude ~kind:RE

  else
    let _ = assert (List.length re_el_qs = 0 && List.length re_el_unproven = 0) in
    unproven

  (* let re_el_qs = filter ~kind:RE_EL all in
  let re_qs = filter ~kind:RE all in
  let re_unproven = filter ~kind:RE unproven in
  let re_el_safe_meta = List.length re_qs > 0 && List.length re_unproven = 0 in
  if re_el_safe_meta then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE
  else if List.length re_el_qs = 0 then
    unproven
  else
    unproven |> exclude ~kind:RE *)

let csv_report_alarms : string -> int
= fun reportfile ->
  let rows = Csv.Rows.load ~has_header:true ~header:csv_header reportfile in
  List.length rows

let mk_vul_report ?(report="vulnerability_report.csv"): Global.t -> QMap.t -> string -> unit
= fun global qmap outdir ->
  let all = QMap.bindings qmap in
  let unproven = get_unproven all in
  let adjusted = adjust_re_alarms all unproven in
  let csv = [csv_header] @ (qlst_to_csv global adjusted) in
  let dir = outdir ^ report in
  Csv.save dir csv

let mk_reg_report : Global.t -> QMap.t -> string -> unit
= fun global qmap outdir ->
  let lst = QMap.bindings qmap in
  let lst = get_proven lst in
  let csv = [csv_header] @ (qlst_to_csv global lst) in
  let dir = outdir ^ "regression_report.csv" in
  Csv.save dir csv

let mk_json_invmap : InvMap.t -> Yojson.Basic.t
= fun invmap ->
  List.map (fun (k,d) ->
    `Assoc [("key", `String (InvMap.to_string_key k));
            ("formula", `String (Parse.to_string_vformula2 d))]
  ) (InvMap.bindings invmap)
  |> (fun res -> `List res)

let mk_json_specmap : SpecMap.t -> Yojson.Basic.t
= fun specmap ->
  List.map (fun (k,(pre,post)) ->
    `Assoc [("key", `String (SpecMap.to_string_key k));
            ("pre", `String (Parse.to_string_vformula2 pre));
            ("post", `String (Parse.to_string_vformula2 post))]
  ) (SpecMap.bindings specmap)
  |> (fun res -> `List res)

let mk_json_report : Global.t -> QMap.t -> QMap.t -> InvMap.t -> SpecMap.t -> unit
= fun global qmap reg_qmap invmap specmap ->
  let vul_json = mk_json_lst qmap in
  let reg_json = mk_json_lst reg_qmap in
  let j =
    `Assoc [("fileName", `String !input);
            ("baseName", `String (Filename.basename !input));
            ("iter", `Int !iter);
            ("time", `Float !Profiler.end_cpu);
            ("errMsg", `Null);

            ("invmap", mk_json_invmap invmap);
            ("specmap", mk_json_specmap specmap);

            ("cei_violated", `Bool !PatternAnalysis.may_violate_cei);
            ("vul_result", `List vul_json);
            ("reg_result", `List reg_json)] in
  let base = snd (BatString.replace ~str:(Filename.basename !input) ~sub:".sol" ~by:"") in
  let outdir = if !outdir = "" then "./output/" else !outdir ^ "/" in
  let full_fname = outdir ^ base ^ ".json" in
  let fp = open_out full_fname in
  Printf.fprintf fp "%s" (Yojson.Basic.pretty_to_string j);
  close_out fp;
  mk_vul_report global qmap outdir;
  if !Chk.reg then mk_reg_report global reg_qmap outdir

let get_qs_from_json_report ~status (fname : string) : Query.ID.t list =
  let module J = Yojson.Basic.Util in
  assert (List.mem status ["all"; "proven"; "unproven"]);
  fname
  |> Yojson.Basic.from_file
  |> J.member "vul_result" |> J.to_list
  |> (if status = "all" then Vocab.id
      else List.filter (fun j -> j |> J.member "status" |> J.to_string = status))
  |> List.map
     (fun j ->
      let kind = j |> J.member "kind" |> J.to_string |> Kind.of_string in
      let loc = raise Vocab.NotImplemented in
      let exp = j |> J.member "exp" |> J.to_string in
      { Query.ID.kind = kind; loc; exp })
