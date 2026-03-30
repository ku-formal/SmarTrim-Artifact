open! Frontend
open Frontend.Lang
open Vlang
open Semantics

(* safety condition: @TU[rcv] \/ @Invest_sum >= eth to be sent *)

let collect_queries (vf : vformula) (path : Paths.t) (stmt : Stmt.t) : Query.t list =
  let invest_sum_entry = Var.label_entry invest_sum in
  let high_value = VInt (Z.pow (Z.of_int 2) 255) in
  match stmt with
  | Call (_lvop, Lv (MemberAccess (e, fname, _, _)), args, _, _, loc)
    when Typ.is_address_kind (get_type_exp e) && List.mem fname [ "transfer"; "send" ] ->
    assert (no_eth_gas_modifiers stmt);
    assert (List.length args = 1);
    let stolen_eth = List.hd args in
    let rcv = convert_aexp e in
    let rcv_trust = VBinRel (VEq, Read (VVar trust_map, rcv), VCond VTrue) in
    (* let invalid_rcv = VBinRel (VEq, rcv, VInt BatBig_int.zero) in *)
    let rcv_invested = VBinRel (VGeq, VVar invest_sum_memo, VVar pay_amount_memo) in
    let entry_has_underflowed_before = VBinRel (VGeq, VVar invest_sum_entry, high_value) in
    let sc = VOr (VOr (rcv_trust, rcv_invested), entry_has_underflowed_before) in
    let vc = Imply (vf, sc) in
    let rcv_src = to_string_exp ~report:true e in
    let stolen_eth_src = to_string_exp ~report:true stolen_eth in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Delayed;
        kind = EL;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = Paths.fkey path;
        sc_src = "";
        attacker_src = rcv_src;
        eth_src = stolen_eth_src;
      };
    ]
  | Extcall { id; ret; fkey; target; args; ether = Some eth; gas; is_static; loc } ->
    ignore (id, ret, fkey, args, is_static, gas);
    let stolen_eth = eth in
    let rcv = convert_aexp target in
    let rcv_trust = VBinRel (VEq, Read (VVar trust_map, rcv), VCond VTrue) in
    let rcv_invested = VBinRel (VGeq, VVar invest_sum_memo, VVar pay_amount_memo) in
    let zero_ether = VBinRel (VEq, convert_aexp stolen_eth, VInt Z.zero) in
    let sc = VOr (VOr (rcv_trust, rcv_invested), zero_ether) in
    let vc = Imply (vf, sc) in
    let rcv_src = to_string_exp ~report:true target in
    let stolen_eth_src = to_string_exp ~report:true stolen_eth in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Delayed;
        kind = EL;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = Paths.fkey path;
        sc_src = "";
        attacker_src = rcv_src;
        eth_src = stolen_eth_src;
      };
    ]
  | Call (_lvop, Lv (Var (fname, _)), args, _, _, loc)
    when List.mem fname [ "selfdestruct"; "suicide" ] ->
    assert (List.length args = 1);
    let () =
      let typ = get_type_exp (List.hd args) in
      assert (Typ.(is_address_kind typ || is_uintkind_or_constint typ))
    in
    assert (no_eth_gas_modifiers stmt);
    let rcv_exp = List.hd args in
    let rcv = convert_aexp rcv_exp in
    let rcv_trust = VBinRel (VEq, Read (VVar trust_map, rcv), VCond VTrue) in
    let rcv_invested = VBinRel (VGeq, VVar invest_sum, Read (VVar eth_map, VVar this_addr)) in
    let zero_ether = VBinRel (VEq, Read (VVar eth_map, VVar this_addr), VInt Z.zero) in
    let sc = VOr (VOr (rcv_trust, rcv_invested), zero_ether) in
    let vc = Imply (vf, sc) in
    let rcv_src = to_string_exp ~report:true rcv_exp in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = EL;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = Paths.fkey path;
        sc_src = "";
        attacker_src = rcv_src;
        eth_src = "address(this).balance";
      };
    ]
  | _ -> []
;;
