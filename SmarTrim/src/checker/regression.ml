open Vlang
open Frontend.Lang
open Semantics

let collect_queries : Global.t -> vformula -> Paths.t -> Stmt.t -> Query.t list =
 fun _global vf path stmt ->
  match stmt with
  | Assert (e, "no_effect", loc) ->
    let sc = Semantics.convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = NO_EFFECT;
        qloc = loc;
        org_q = Org_exp e;
        path;
        src_f = path.fkey;
        sc_src;
        attacker_src = "";
        eth_src = "";
      };
    ]
  | Assert (e, "assign_const", loc) ->
    let sc = Semantics.convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = ASSIGN_CONST;
        qloc = loc;
        org_q = Org_exp e;
        path;
        src_f = path.fkey;
        sc_src;
        attacker_src = "";
        eth_src = "";
      };
    ]
  | Assert (e, "deadcode", loc) ->
    let sc = Semantics.convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = DEAD;
        qloc = loc;
        org_q = Org_exp e;
        path;
        src_f = path.fkey;
        sc_src;
        attacker_src = "";
        eth_src = "";
      };
    ]
  (* | Assert (e,"reentrancy",loc) ->
    let sc = convert_bexp e in
    let vc = Imply (vf, sc) in
    [{vc=vc; vc2=sc; kind=RE_ENT; loc=loc; org_q=Org_Exp e; path=path; src_f=Path.get_fkey path; sc_src=""}] *)
  | _ -> []
;;
