(* open Reports
open Validate

let leak_unsafe2 =
  {|library SmarTest_event {
  event smartest_check (bool safety_cond);
  function check_wrapper (bool b) internal {emit smartest_check(b);}
  event smartest_check2 (uint attacker_balance, uint stolen_money);
  function check_wrapper2 (uint bal, uint money) internal {emit smartest_check2(bal, money);}
}
contract Example {
    address owner;

    constructor () {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require (msg.sender == owner);
        _;
    }

    function setOwner (address newOwner) public {
        owner = newOwner; 
    }

    function () external payable {
    }

    function exploit (address attacker, uint value) public onlyOwner {
SmarTest_event.check_wrapper2(attacker.balance, value);         attacker.transfer (value);
    }
}
contract SMARTEST is Example {
  constructor () Example () public payable { }

}
|}
;;

let leak_unsafe2_good : Exploit_r.Tseq.t =
  let open Exploit_r in
  {
    tseq_info = None;
    main_contract = "SMARTEST";
    compiler_version = "0.4.26";
    kind = EL;
    this_address = "0xdffbfff0fffffffffff7fcc9cc8fb860e7ff203f";
    init_eth = "1000000000000000000";
    send_eth = "1000000000000000000000";
    malicious_receiver = Some "0x0004000f00000000000803363370479f1800dfc0";
    stolen_money = Some "1000000000000000000";
    trusted =
      [
        ("0x0004000f00000000000803363370479f1800dfc0", false);
        ("0x0004004f00000000000803363350479f18001bc0", false);
        ("0x0004004f00000000000823363370479f1800dfc0", false);
        ("0xdffbfff0fffffffffff7fcc9cc8fb860e7ff203f", true);
        ("0xfffbffb0fffffffffff7dcc9cc8fb860e7ff203f", true);
      ];
    delayed_safety = true;
    tseq =
      [
        Tx.make ~tid:0 ~contract_name:"SMARTEST" ~fname:"Example"
          ~tx_args:(Tx_args.make ~msg_sender:"0xfffbffb0fffffffffff7dcc9cc8fb860e7ff203f" ())
          ~fn_kind:Constr ();
        Tx.make ~tid:1 ~contract_name:"SMARTEST" ~fname:"setOwner"
          ~args:[ Elem ("0x0004004f00000000000823363370479f1800dfc0", T_address) ]
          ~tx_args:(Tx_args.make ~msg_sender:"0x0004004f00000000000803363350479f18001bc0" ())
          ();
        Tx.make ~tid:2 ~contract_name:"SMARTEST" ~fname:"exploit"
          ~args:
            [
              Elem ("0x0004000f00000000000803363370479f1800dfc0", T_address);
              Elem ("1000000000000000000", T_uint 256);
            ]
          ~tx_args:(Tx_args.make ~msg_sender:"0x0004004f00000000000823363370479f1800dfc0" ())
          ();
      ];
  }
;;

let%test "leak-unsafe2-good" =
  let tseq = Concrete.Tseq.to_origin leak_unsafe2_good in
  Run.main leak_unsafe2 tseq = Ok ()
;;

let%test "leak-unsafe2-succ-small-withdraw" =
  let tseq = leak_unsafe2_good in
  let modifier (tx : Exploit_r.Tx.t) =
    {
      tx with
      args =
        [ Elem ("0x0004000f00000000000803363370479f1800dfc0", T_address); Elem ("1", T_uint 256) ];
    }
  in
  let tx_list = BatList.modify_at 2 modifier tseq.tseq in
  let tseq = { tseq with tseq = tx_list } in
  let tseq = Concrete.Tseq.to_origin tseq in
  Run.main leak_unsafe2 tseq = Ok ()
;;

let%test "leak-unsafe2-fail-send-to-me" =
  let tseq = leak_unsafe2_good in
  let modifier (tx : Exploit_r.Tx.t) =
    {
      tx with
      args = [ Elem (tseq.this_address, T_address); Elem ("1000000000000000000", T_uint 256) ];
    }
  in
  let tx_list = BatList.modify_at 2 modifier tseq.tseq in
  let tseq = { tseq with tseq = tx_list } in
  let tseq = Concrete.Tseq.to_origin tseq in
  Run.main leak_unsafe2 tseq = Error Vd_util.Vd_error.TEST_FAILED
;;

let%test "leak-unsafe2-fail-impossible-withdraw" =
  let tseq = leak_unsafe2_good in
  let modifier (tx : Exploit_r.Tx.t) =
    {
      tx with
      args =
        [
          Elem ("0x0004000f00000000000803363370479f1800dfc0", T_address);
          Elem ("1000000000000000001", T_uint 256);
        ];
    }
  in
  let tx_list = BatList.modify_at 2 modifier tseq.tseq in
  let tseq = { tseq with tseq = tx_list } in
  let tseq = Concrete.Tseq.to_origin tseq in
  Run.main leak_unsafe2 tseq = Error Vd_util.Vd_error.TEST_FAILED
;;

let%test "leak-unsafe2-fail-impossible-withdraw-init-eth" =
  let tseq = leak_unsafe2_good in
  let tseq = { tseq with init_eth = "422" } in
  let tseq = Concrete.Tseq.to_origin tseq in
  Run.main leak_unsafe2 tseq = Error Vd_util.Vd_error.TEST_FAILED
;;

let%test "leak-unsafe2-fail-unseen-address" =
  let tseq = leak_unsafe2_good in
  let modifier (tx : Exploit_r.Tx.t) =
    {
      tx with
      args =
        [
          Elem ("0x0004000f00000000000803363370479f1800dfc9", T_address);
          Elem ("1000000000000000000", T_uint 256);
        ];
    }
  in
  let tx_list = BatList.modify_at 2 modifier tseq.tseq in
  let tseq = { tseq with tseq = tx_list } in
  let tseq = Concrete.Tseq.to_origin tseq in
  Run.main leak_unsafe2 tseq = Error Vd_util.Vd_error.TEST_FAILED
;; *)
