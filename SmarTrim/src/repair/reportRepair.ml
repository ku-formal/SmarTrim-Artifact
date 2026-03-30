open WorkspaceRepair
open Patch
open RunRepair
open Options

let cost (_,_,_,found_time) = found_time
let compare x y = Stdlib.compare (cost x) (cost y)

let filter ~(kind : Frontend.Kind.t) alarms = BatSet.filter (fun (k',_,_,_) -> kind = k') alarms

let find_first_last_iter iter_lst =
  if List.length iter_lst = 0 then
    (`Null, `Null, "n/a", "n/a")
  else
    let first, last = BatList.first iter_lst, BatList.last iter_lst in
    (`Int first, `Int last, string_of_int first, string_of_int last)

let find_first_last_time time_lst =
  if List.length time_lst = 0 then
    (`Null, `Null, "n/a", "n/a")
  else
    let first, last = BatList.first time_lst, BatList.last time_lst in
    (`Float first, `Float last, string_of_float first, string_of_float last)

let get_alarm_pat_num solutions =
  if List.length solutions = 0 then (None, None)
  else
    (* let alarm_num = List.map (fun (f,p,i,t) -> f |> Filename.dirname |> get_vr |> FixTarget.get_alarms |> BatSet.cardinal) solutions in *)
    let alarm = List.map (fun (f,_,_,_) -> f |> Filename.dirname |> get_vr |> FixTarget.get_alarms2) solutions in
    let alarm_num = List.map (fun (alarm,alarm_wo_leak) -> (BatSet.cardinal alarm, BatSet.cardinal alarm_wo_leak)) alarm in
    let _ = assert (BatSet.cardinal (BatSet.of_list (List.map fst alarm_num)) = 1) in
    let alarm_num = List.hd alarm_num in
    let num, num_wo_leak = fst alarm_num, snd alarm_num in
    (Some num, Some num_wo_leak)

let get_top_k_info k sols =
  let _ = assert (k>=1) in
  if k > List.length sols then None
  else
    BatList.at sols (k-1)
    |> (fun (_,p,iter,time) -> Some (List.length p, iter, time))

let json_of_rank_info info =
  match info with
  | None -> `Null
  | Some (edit,iter,time) ->
    `Assoc [("edit", `Int edit); ("iter", `Int iter); ("time", `Float time)]

let print : (file * patch * int * float) list -> unit
= fun solutions ->
  let patch_sizes = BatSet.of_list (List.map (fun (_,p,_,_) -> Patch.length2 p) solutions) in
  let _ = assert ((List.length solutions = 0 && BatSet.cardinal patch_sizes = 0) ||
                  (List.length solutions > 0 && BatSet.cardinal patch_sizes = 1)) in

  let solutions = List.sort compare solutions in
  (* let _ =
    assert (List.for_all (fun (f,p,i,t) ->
      f |> Filename.dirname |> get_vr |> FixTarget.get_regressions |> BatSet.cardinal = 0) solutions) in *)

  let alarm_org, alarm_org_wo_leak = FixTarget.get_alarms2 (get_vr !outdir) in
  let alarm_org_num, alarm_org_wo_leak_num = BatSet.cardinal alarm_org, BatSet.cardinal alarm_org_wo_leak in
  let alarm_pat_num, alarm_pat_wo_leak_num = get_alarm_pat_num solutions in

  (* let iter_lst = List.map (fun (_,_,i,_) -> i) solutions |> List.sort Stdlib.compare in
  let time_lst = List.map (fun (_,_,_,t) -> t) solutions |> List.sort Stdlib.compare in
  let (first_iter_j, last_iter_j, first_iter, last_iter) = find_first_last_iter iter_lst in
  let (first_time_j, last_time_j, first_time, last_time) = find_first_last_time time_lst in *)
  let edits = List.map (fun (_,p,_,_) -> List.length p) solutions in
 
  let rank1_info = get_top_k_info 1 solutions in
  let rank2_info = get_top_k_info 2 solutions in
  let rank3_info = get_top_k_info 3 solutions in

  print_endline "\n========== Statistics ==========";
  print_endline ("# Iter                    : " ^ string_of_int !RunRepair.iter);
  print_endline ("# Alarm (orginal)         : " ^ string_of_int alarm_org_num);
  if !Chk.io    then print_endline ("- integer over/underflow  : " ^ string_of_int (alarm_org |> filter ~kind:IO |> BatSet.cardinal));
  if !Chk.el  then print_endline ("- ether-leaking           : " ^ string_of_int (alarm_org |> filter ~kind:EL |> BatSet.cardinal));
  if !Chk.su  then print_endline ("- kill-anyone             : " ^ string_of_int (alarm_org |> filter ~kind:SU |> BatSet.cardinal));
  if !Chk.re    then print_endline ("- reentrancy-leaking      : " ^ string_of_int (alarm_org |> filter ~kind:RE_EL |> BatSet.cardinal));
  if !Chk.re    then print_endline ("- reentrancy              : " ^ string_of_int (alarm_org |> filter ~kind:RE |> BatSet.cardinal));
  if !Chk.tx    then print_endline ("- tx.origin               : " ^ string_of_int (alarm_org |> filter ~kind:TX_ORG |> BatSet.cardinal));

  print_endline ("# Alarm (patch)           : " ^ (match alarm_pat_num with None -> "n/a" | Some n -> string_of_int n));
  print_endline ("# Alarm wo leak (patch)   : " ^ (match alarm_pat_wo_leak_num with None -> "n/a" | Some n -> string_of_int n));

  print_endline ("# Edit (min,max)          : " ^ if List.length edits = 0 then "n/a" else string_of_int (BatList.min edits) ^ ", " ^ string_of_int (BatList.max edits));
  print_endline ("# Patch                   : " ^ string_of_int (List.length solutions));

  print_endline ("- First solution          : " ^ if !RunRepair.first_sol_found_iter = 0 then "n/a" else !RunRepair.first_sol_dir);
  print_endline ("- First solution (iter)   : " ^ if !RunRepair.first_sol_found_iter = 0 then "n/a" else string_of_int (!RunRepair.first_sol_found_iter));
  print_endline ("- First solution (time)   : " ^ if !RunRepair.first_sol_found_iter = 0 then "n/a" else string_of_float (!RunRepair.first_sol_found_time));

  print_endline ("# Uncompiled candidate    : " ^ string_of_int (List.length !RunRepair.uncompiled));
  print_endline ("# Verifier error          : " ^ string_of_int (List.length !RunRepair.verify_err));

  if List.length solutions > 0 then
   (print_endline "\n========== Patch info ==========";
    List.iteri (fun i (f,p,iter,t) ->
      print_endline (
        "- " ^ string_of_int (i+1) ^ ", " ^
        "Size: " ^ string_of_float (Patch.length2 p) ^ ", " ^
        "Edit: " ^ string_of_int (List.length p) ^ ", " ^
        "Iter: " ^ string_of_int iter ^ ", " ^
        "Time: " ^ string_of_float t ^ ", " ^ f
      );
    ) solutions);

  if List.length solutions = 0 then
   (print_endline "\n========== Patch info ==========";
    print_endline "No patch generated");

  if List.length !RunRepair.patch_err > 0 then
   (print_endline "\n========== Patch Exceptions ==========";
    List.iter (fun f -> print_endline f) !RunRepair.patch_err);

  if List.length !RunRepair.uncompiled > 0 then
   (print_endline "\n========== Uncompiled ==========";
    List.iter (fun f -> print_endline f) !RunRepair.uncompiled);

  if List.length !RunRepair.verify_err > 0 then
   (print_endline "\n======== Verifier error ========";
    List.iter (fun f -> print_endline f) !RunRepair.verify_err);

  if not !mk_report then ()
  else
  let detail =
    List.mapi (fun i (f,p,iter,t) ->
      `Assoc [("no", `Int (i+1));
              ("dir", `String f);
              ("found_iter", `Int iter);
              ("found_time", `Float t);
              ("edit", `Int (List.length p));
              ("alarm", `Int (BatSet.cardinal (WorkspaceRepair.get_vul_alarms f)))]) solutions in
  let output =
    `Assoc [("fileName", `String !input);
            ("baseName", `String (Filename.basename !input));
            ("errMsg", `Null);
            ("time", `Float !Profiler.end_real);
            ("iter", `Int !RunRepair.iter);
            ("alarm_org", `Int alarm_org_num);
            ("alarm_org_wo_leak", `Int alarm_org_wo_leak_num);
            ("alarm_pat", (match alarm_pat_num with None -> `Null | Some n -> `Int n));
            ("alarm_pat_wo_leak", (match alarm_pat_wo_leak_num with None -> `Null | Some n -> `Int n));
            ("rank1", json_of_rank_info rank1_info);
            ("rank2", json_of_rank_info rank2_info);
            ("rank3", json_of_rank_info rank3_info);
            ("edit_min", if List.length edits = 0 then `Null else `Int (BatList.min edits));
            ("edit_max", if List.length edits = 0 then `Null else `Int (BatList.max edits));
            ("patch_found", `Int (List.length solutions));
            ("first_sol_iter", if !RunRepair.first_sol_found_iter = 0 then `Null else `Int !RunRepair.first_sol_found_iter);
            ("first_sol_time", if !RunRepair.first_sol_found_iter = 0 then `Null else `Float !RunRepair.first_sol_found_time);
            ("first_sol_dir", if !RunRepair.first_sol_found_iter = 0 then `Null else `String !RunRepair.first_sol_dir);
            ("uncompiled", `Int (List.length !RunRepair.uncompiled));
            ("patch_err", `Int (List.length !RunRepair.patch_err));
            ("verify_err", `Int (List.length !RunRepair.verify_err));
            ("detail", `List detail)] in
  let f = open_out (!outdir ^ "/summary.json") in
  Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string output);
  close_out f
