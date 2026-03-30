open Vlang
open! Frontend
open Frontend.Lang
open Options
open Semantics
open! Vocab

let collect_queries ?(extern_ctx = false) (global : Global.t) (vf : vformula) (path : Paths.t)
    (g : Cfg.t) (stmt : Stmt.t) : Query.t list =
  (* let mid = Path.get_mid (snd path) in *)
  let fkey = Paths.fkey path in
  let f = FuncMap.find fkey global.fmap in
  let defset = FuncDefUse.find_d fkey global.f_defuse in
  let fields = Global.get_all_fields global in
  let field_names = List.map fst fields in
  let g_defs =
    BatSet.filter
      (fun d ->
        List.mem d (List.map fst global.global_vars) || d = "@Invest_sum" || List.mem d field_names)
      defset
  in
  (* let b0 = (is_public_func f || is_external_func f) && BatList.last mid = node in *)
  (* check at the end of basic paths only, for efficiency (vs. all assignments) *)
  let b1 = extern_ctx in
  let b2 =
    (not (BatSet.is_empty g_defs)) || ((not (Func.is_modifier f)) && not (Func.is_view_pure f))
  in
  let b3 = !PatternAnalysis.may_violate_cei || !re_safety_enforce in
  let b4 =
    FuncMap.is_internal_call_node global.fmap global.cnames (BatList.last (Paths.basic_path path)) g
  in
  (* re/reentrancy_bonus *)
  let re_state_check_cond = (* b0 && *) b1 && b2 && b3 && not b4 in

  match stmt with
  | Revert -> []
  | Return (_, loc) when loc.line < 0 -> []
  | _
    when re_state_check_cond
         && PatternAnalysis.is_state_manipulating_assign global stmt
         && !mode = Verify ->
    let sc = VBinRel (VEq, Read (VVar trust_map, VVar msg_sender), VCond VTrue) in
    let vc = Imply (vf, sc) in
    let f = FuncMap.find fkey global.fmap in
    let line = f.info.floc.line in
    if fkey.contract = !main_contract then
      [
        {
          vc;
          vc2 = sc;
          vc_kind = `Delayed;
          kind = RE;
          qloc = Query.line2loc line;
          org_q = Org_func fkey;
          path;
          src_f = fkey;
          sc_src = "";
          attacker_src = "";
          eth_src = "";
        };
      ]
    else []
  | Extcall { id; ret; fkey; target; args; ether = Some eth; gas; is_static; loc } ->
    ignore (id, ret, args, gas, is_static);
    let stolen_eth = eth in
    let rcv = Semantics.convert_aexp target in
    let rcv_trust = VBinRel (VEq, Read (VVar trust_map, rcv), VCond VTrue) in
    let rcv_invested = VBinRel (VGeq, VVar invest_sum_memo, VVar pay_amount_memo) in
    let zero_ether = VBinRel (VEq, Semantics.convert_aexp stolen_eth, VInt Z.zero) in
    let sc = VOr (VOr (rcv_trust, rcv_invested), zero_ether) in
    let vc = Imply (vf, sc) in
    let rcv_src = to_string_exp ~report:true target in
    let stolen_eth_src = to_string_exp ~report:true stolen_eth in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Delayed;
        kind = RE_EL;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = fkey;
        sc_src = "";
        attacker_src = rcv_src;
        eth_src = stolen_eth_src;
      };
    ]
  | Call (_, Lv (MemberAccess (e, fname, _, _)), args, _, _, loc)
    when Typ.is_address_kind (get_type_exp e) && List.mem fname [ "transfer"; "send" ] && b1 ->
    assert (no_eth_gas_modifiers stmt);
    assert (List.length args = 1);
    let stolen_eth = List.hd args in
    let rcv = Semantics.convert_aexp e in
    let rcv_trust = VBinRel (VEq, Read (VVar trust_map, rcv), VCond VTrue) in
    let rcv_invested = VBinRel (VGeq, VVar invest_sum_memo, VVar pay_amount_memo) in
    let zero_ether = VBinRel (VEq, Semantics.convert_aexp stolen_eth, VInt Z.zero) in
    let sc = VOr (VOr (rcv_trust, rcv_invested), zero_ether) in
    let vc = Imply (vf, sc) in
    let rcv_src = to_string_exp ~report:true e in
    let stolen_eth_src = to_string_exp ~report:true stolen_eth in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Delayed;
        kind = RE_EL;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = Paths.fkey path;
        sc_src = "";
        attacker_src = rcv_src;
        eth_src = stolen_eth_src;
      };
    ]
  | _ -> []
;;
