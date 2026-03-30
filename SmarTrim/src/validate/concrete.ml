open Reports
open Reports.Exploit_r
module Typ = Exploit_r.Typ
module Value = Exploit_r.Value
module Reent_info = Exploit_r.Reent_info

open struct
  (** show value *)
  let sv = Value.show_solexp

  let map = Option.map

  (** string to address *)
  let s2a s = Value.Elem (s, T_address)

  (** string to uint256 *)
  let s2u256 s = Value.Elem (s, T_uint 256)
end

module Tx_args = struct
  type t = {
    msg_sender : Value.t;
    msg_value : Value.t;
    tx_origin : Value.t option;
    block_coinbase : Value.t option;
    block_timestamp : Value.t option;
    block_number : Value.t option;
  }
  [@@deriving show { with_path = false }]

  let to_compact (a : t) : Tx_args.t =
    {
      msg_sender = sv a.msg_sender;
      msg_value = sv a.msg_value;
      tx_origin = map sv a.tx_origin;
      block_coinbase = map sv a.block_coinbase;
      block_timestamp = map sv a.block_timestamp;
      block_number = map sv a.block_number;
    }
  ;;

  let to_origin (a : Tx_args.t) : t =
    {
      msg_sender = s2a a.msg_sender;
      msg_value = s2u256 a.msg_value;
      tx_origin = map s2a a.tx_origin;
      block_coinbase = map s2u256 a.block_coinbase;
      block_timestamp = map s2u256 a.block_timestamp;
      block_number = map s2u256 a.block_number;
    }
  ;;
end

(** Transactions, concrete *)
module Tx = struct
  type t = {
    tid : int;
    contract_name : string;
    fname : string;
    args : Value.t list;
    tx_args : Tx_args.t;
    reenter : (Reent_info.t * t list) list;
    is_contract_addr : bool;
    fn_kind : Fn_kind.t;
  }
  [@@deriving show { with_path = false }]

  let get_encode tx =
    let argtyps = List.map Value.to_typ tx.args in
    let typnames = List.map Typ.Ez.show argtyps in
    let s = String.concat "," typnames in
    Pp.spf {|"%s(%s)"|} tx.fname s
  ;;

  let rec to_compact
      ({ tid; contract_name; fname; args; tx_args; reenter; is_contract_addr; fn_kind } : t) : Tx.t
      =
    let reenter = List.map (T2.map2 (List.map to_compact)) reenter in
    {
      tid;
      contract_name;
      fname;
      args;
      tx_args = Tx_args.to_compact tx_args;
      reenter;
      is_contract_addr;
      fn_kind;
    }
  ;;

  let rec to_origin
      ({ tid; contract_name; fname; args; tx_args; reenter; is_contract_addr; fn_kind } : Tx.t) : t
      =
    let reenter = List.map (T2.map2 (List.map to_origin)) reenter in
    {
      tid;
      contract_name;
      fname;
      args;
      tx_args = Tx_args.to_origin tx_args;
      reenter;
      is_contract_addr;
      fn_kind;
    }
  ;;
end

module Tseq = struct
  type t = {
    tseq_info : Tseq_info.t option; [@yojson.option]
    main_contract : string;
    compiler_version : string;
    kind : Kind.t;
    this_address : Value.t;
    init_eth : Value.t;
    send_eth : Value.t;
    malicious_receiver : Value.t option; [@yojson.option]
    stolen_money : Value.t option; [@yojson.option]
    trusted : (Value.t * bool) list;
    balances : (Value.t * Value.t) list;
    delayed_safety : bool;
    tseq : Tx.t list;
  }
  [@@deriving show { with_path = false }]

  let to_compact (s : t) : Tseq.t =
    {
      tseq_info = s.tseq_info;
      main_contract = s.main_contract;
      compiler_version = s.compiler_version;
      kind = s.kind;
      this_address = sv s.this_address;
      init_eth = sv s.init_eth;
      send_eth = sv s.send_eth;
      malicious_receiver = map sv s.malicious_receiver;
      stolen_money = map sv s.stolen_money;
      trusted = List.map (fun (v, b) -> (sv v, b)) s.trusted;
      balances = List.map (T2.mapn sv) s.balances;
      delayed_safety = s.delayed_safety;
      tseq = List.map Tx.to_compact s.tseq;
    }
  ;;

  let to_origin (s : Tseq.t) : t =
    {
      tseq_info = s.tseq_info;
      main_contract = s.main_contract;
      compiler_version = s.compiler_version;
      kind = s.kind;
      this_address = s2a s.this_address;
      init_eth = s2u256 s.init_eth;
      send_eth = s2u256 s.send_eth;
      malicious_receiver = map s2a s.malicious_receiver;
      stolen_money = map s2u256 s.stolen_money;
      trusted = List.map (fun (v, b) -> (s2a v, b)) s.trusted;
      balances = List.map (T2.mapn s2a) s.balances;
      delayed_safety = s.delayed_safety;
      tseq = List.map Tx.to_origin s.tseq;
    }
  ;;

  let attackers (s : t) : Value.t list =
    let module S = Set.Make (Value) in
    let rec aux (tx : Tx.t) : S.t =
      let reent_infos, reent_tseqs = List.split tx.reenter in
      let txs = List.concat reent_tseqs in
      let attacks = List.map (Field.get Reent_info.Fields.called_attacker) reent_infos in
      let attacks = S.of_list attacks in
      List.fold_left (fun acc tx -> S.union acc (aux tx)) attacks txs
    in
    let s = List.fold_left (fun acc tx -> S.union acc (aux tx)) S.empty s.tseq in
    S.to_list s
  ;;
end
