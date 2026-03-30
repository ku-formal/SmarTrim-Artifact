open Frontend.Lang
open Vlang
open Semantics

let collect_queries : vformula -> Paths.t -> Stmt.t -> Query.t list =
 fun vf path stmt ->
  match stmt with
  | Call (_lvop, Lv (Var (fname, _)), _, _, _, loc)
    when List.mem fname [ "selfdestruct"; "suicide" ] ->
    assert (no_eth_gas_modifiers stmt);
    let sc =
      VBinRel (VEq, Read (VVar trust_map, VVar ("msg.sender", EType Address)), VCond VTrue)
    in
    let vc = Imply (vf, sc) in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = SU;
        qloc = loc;
        org_q = Org_stmt stmt;
        path;
        src_f = Paths.fkey path;
        sc_src = "";
        attacker_src = "";
        eth_src = "";
      };
    ]
  | _ -> []
;;
