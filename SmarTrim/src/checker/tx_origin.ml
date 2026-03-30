open Vlang
open! Frontend
open Frontend.Lang
open Semantics

let collect_queries : Global.t -> vformula -> Paths.t -> Cfg.t -> Node.t -> Query.t list =
 fun global vf path g node ->
  let func = FuncMap.find (Paths.fkey path) global.fmap in
  let params = List.map (fun (v, vinfo) -> (v, vinfo.vtyp)) func.params in
  let params = List.filter (fun (_, typ) -> Typ.is_address typ) params in
  let stmt = Cfg.find_stmt node g in
  match stmt with
  | Assume (BinOp (bop, e1, e2, einfo), _)
    when einfo.loc.line > 0
         && Typ.is_address_kind (get_type_exp e1)
         && Typ.is_address_kind (get_type_exp e2)
         && (bop = Eq || bop = NEq) ->
    (* other bops possible *)
    (* cf) line 893: https://ww6.etherscan.io/address/0x5f65b93645dc893160fd98c482f93e30850e7bba *)
    let sc1 = VBinRel (VEq, convert_aexp e1, VVar msg_sender) in
    let sc2 = VBinRel (VEq, convert_aexp e2, VVar msg_sender) in
    let sc3 = VBinRel (VEq, convert_aexp e1, VCast (EType Address, VInt Z.zero)) in
    let sc4 = VBinRel (VEq, convert_aexp e2, VCast (EType Address, VInt Z.zero)) in
    let sc5 =
      List.fold_left
        (fun acc param ->
          let f' =
            VOr
              ( VBinRel (VEq, convert_aexp e1, VVar param),
                VBinRel (VEq, convert_aexp e2, VVar param) )
          in
          if equal_vf acc VTrue then f' else VOr (acc, f'))
        VTrue params
    in
    let sc = VOr (VOr (VOr (sc1, sc2), VOr (sc3, sc4)), sc5) in
    let vc = Imply (vf, sc) in
    [
      {
        vc;
        vc2 = sc;
        vc_kind = `Basic;
        kind = TX_ORG;
        qloc = einfo.loc;
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
