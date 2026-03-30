open Concrete

let v0_5_0 = Solc.Ver.mk 0 5 0
let v0_6_0 = Solc.Ver.mk 0 6 0
let v0_6_2 = Solc.Ver.mk 0 6 2
let import_forge_tester v = if v >= v0_6_2 then {|import {Test} from "forge-std/Test.sol";|} else ""
let inherit_test v = if v >= v0_6_2 then "is Test " else ""

let vm_interface v =
  if v >= v0_6_2 then ""
  else
    {%string|interface Vm {
  function prank(address) external;
  function prank(address, address) external;
  function deal(address, uint256) external;
  function expectRevert(bytes calldata) external;
  function recordLogs() external;
  function getRecordedLogs() external returns (bytes32[] memory);
  function coinbase(address newCoinbase) external;
  function warp(uint256) external;
  function roll(uint256 newHeight) external;
  function expectEmit() external;
  function expectRevert() external;
  function getCode(string calldata) external returns (bytes memory);
  function etch(address who, bytes calldata) external;
}|}
;;

let asmcall v =
  if v >= v0_5_0 then ""
  else
    {%string|  function asmcall(address target, uint256 value) public 
  returns (bool success, bytes memory returndata) {
    bytes memory input = "";

    assembly {
      let ptr := mload(0x40)
      success := call(gas, target, value, add(input, 0x20), mload(input), 0, 0)

      let size := returndatasize
      returndata := mload(0x40)
      mstore(returndata, size)
      returndatacopy(add(returndata, 0x20), 0, size)
      mstore(0x40, add(add(returndata, 0x20), size))
    }
  }|}
;;

let deploycodeto v =
  if v >= v0_6_2 then ""
  else
    let addresscall =
      if v >= v0_6_2 then {|where.call{ value: value }("")|}
      else if v >= v0_5_0 then {|where.call.value(value)("")|}
      else "asmcall(where, value)"
    in
    {%string|  function deployCodeTo(string memory what, bytes memory args, uint256 value, address where) internal {
    bytes memory creationCode = vm.getCode(what);
    vm.etch(where, abi.encodePacked(creationCode, args));
    (bool success, bytes memory runtimeBytecode) = %{addresscall};
    require(success, "deployCodeTo(string,bytes,uint256,address): Failed to create runtime bytecode.");
    vm.etch(where, runtimeBytecode);
  }|}
;;

let declare_vm v =
  if v >= v0_6_2 then "" else "Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);"
;;

let header (v : Solc.Ver.t) =
  {%string|// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/Counter.sol";
%{import_forge_tester v}

%{vm_interface v}

contract DepthControl {
  uint public tx_count = 0;
  uint public re_count = 0;
  uint public ord_count = 0;
  
  function inc_tx() internal {
    tx_count += 1;
    ord_count = 0;
  }
  function inc_ord() external {
    ord_count += 1;
  }
  function inc_re() external {
    re_count += 1;
  }
  function dec_re() external {
    re_count -= 1;
  }
}

contract SmarTestTestUtil %{inherit_test v}{
  %{declare_vm v}
  
%{asmcall v}
  
%{deploycodeto v}

}|}
;;

(** show value *)
let sv v = Value.show_solexp v

(** show typ *)
let st = Concrete.Typ.Ez.show

(** show typ of value *)
let stv v = st @@ Value.to_typ v

let pad n = String.repeat " " n

let value_modifier (tseq : Tseq.t) (tx : Tx.t) =
  let v = tseq.compiler_version in
  let v = Solc.Ver.of_string_exn v in
  let value_string = sv tx.tx_args.msg_value in
  if v < v0_6_2 then {%string|.value(%{value_string})|} else {%string|{ value: %{value_string} }|}
;;

let init_balance_and_trusts (tseq : Tseq.t) =
  let f (v, trusted) =
    if Value.compare v tseq.this_address = 0 then ""
    else
      let s = Value.show_solexp v in
      if trusted || Reports.Exploit_r.Kind.equal tseq.kind IO then
        (* temporal, kind = IO is not accurate *)
        {%string|vm.deal(%{s}, initialBalance);
%{pad 4}deployCitizenTo(0, address(%{s}));|}
      else
        {%string|vm.deal(%{s}, initialBalance);
%{pad 4}untrusted.push(%{s});
%{pad 4}untrust_map[%{s}] = true;|}
  in
  tseq.trusted |> List.map f |> String.concat "\n    "
;;

let init_attackers (tseq : Tseq.t) =
  let attackers = Tseq.attackers tseq in
  let l =
    List.map (fun (v : Value.t) -> {%string|deployAttackTo(0, address(%{sv v}));|}) attackers
  in
  String.concat "\n    " l
;;

let calldatas n =
  let f i =
    {%string|beforeTestCalldata[%{(i + 1)#Int}] = abi.encodePacked(this.setScenario%{i#Int}.selector);|}
  in
  let sl = List.map f (List.init n id) in
  let s = String.concat "\n      " sl in
  {%string|  function beforeTestSetup(bytes4 testSelector) public pure 
  returns (bytes[] memory beforeTestCalldata) {
    if (testSelector == this.testScenario.selector) {
      beforeTestCalldata = new bytes[](%{(n + 1)#Int});
      beforeTestCalldata[0] = abi.encodePacked(this.setScenarioInit.selector);
      %{s}
    }
  }|}
;;

let coinbase_setter (tx_args : Tx_args.t) : string =
  match tx_args.block_coinbase with
  | Some coinbase -> {%string|vm.coinbase(%{sv coinbase});|}
  | None -> ""
;;

let warp_setter (tx_args : Tx_args.t) : string =
  match tx_args.block_timestamp with Some time -> {%string|vm.warp(%{sv time});|} | None -> ""
;;

let roll_setter (tx_args : Tx_args.t) : string =
  match tx_args.block_number with Some time -> {%string|vm.roll(%{sv time});|} | None -> ""
;;

let prank_template (tx : Tx.t) =
  match tx.tx_args.tx_origin with
  | Some tx_origin ->
    {%string|vm.prank(address(%{sv tx.tx_args.msg_sender}), address(%{sv tx_origin}));|}
  | None -> {%string|vm.prank(address(%{sv tx.tx_args.msg_sender}));|}
;;

let tx_prepare_template (tx : Tx.t) =
  let c = coinbase_setter tx.tx_args in
  let w = warp_setter tx.tx_args in
  let r = roll_setter tx.tx_args in
  let p = prank_template tx in
  let l = List.filter (not % String.is_empty) [ c; w; r; p ] in
  let s = String.concat "\n    " l in
  s
;;

(** {b Returns} declaration contents, function call contents *)
let tx_make_args (tx : Tx.t) =
  let f (i : int) (v : Value.t) : string * string list =
    match v with
    | Array (l, _) ->
      let n = List.length l in
      let tempvar_name = {%string|temp_%{tx.tid#Int}_%{i#Int}|} in
      let decl : string =
        {%string|%{pad 4}%{stv v} memory %{tempvar_name} = new %{stv v}(%{n#Int});|}
      in
      let assigns : string list =
        l |> List.mapi (fun i v -> {%string|%{pad 4}%{tempvar_name}[%{i#Int}] = %{sv v};|})
      in
      (tempvar_name, decl :: assigns)
    | Elem (s, T_dbytes) ->
      let tempvar_name = {%string|temp_%{tx.tid#Int}_%{i#Int}|} in
      let decl = {%string|%{pad 4}bytes memory %{tempvar_name} = %{s};|} in
      (tempvar_name, [ decl ])
    | _ -> (sv v, [])
  in
  let expr_strs, decls = List.mapi f tx.args |> List.split in
  let decl_strs : string = decls |> List.concat |> String.concat "" in
  let args_inside_paren : string = String.concat ", " expr_strs in
  (decl_strs, args_inside_paren)
;;

(** Usage: [constructor_content contract_id args_inside_paren] *)
let constructor_content (tseq : Tseq.t) (tx : Tx.t) args =
  let c = tseq.main_contract in
  let value_string = sv tx.tx_args.msg_value in
  {%string|%{pad 4}//victim = (new %{c})%{value_modifier tseq tx}(%{args});
%{pad 4}deployCodeTo("%{c}", abi.encode(%{args}), %{value_string}, %{Value.show_solexp tseq.this_address});
%{pad 4}vm.deal(address(victim), address(victim).balance + initialVictimBalance);|}
;;

(** [fallback_content tid value] *)
let fallback_content (tseq : Tseq.t) (tx : Tx.t) =
  let tid = tx.tid in
  let v = tx.tx_args.msg_value in
  let success_return =
    if Solc.Ver.(of_string_exn tseq.compiler_version >= v0_5_0) then
      {%string|(bool success_%{tid#Int},)|}
    else {%string|bool success_%{tid#Int}|}
  in
  if Value.is_integer_zero v then
    {%string|%{pad 4}%{success_return} = address(victim).call("");
%{pad 4}require(success_%{tid#Int}, "validator: unexpected fallback revert");|}
  else
    {%string|%{pad 4}%{success_return} = address(victim).call%{value_modifier tseq tx}("");
%{pad 4}require(success_%{tid#Int}, "validator: unexpected fallback revert");|}
;;

(** [receive_content tid value] *)
let receive_content (tseq : Tseq.t) (tx : Tx.t) =
  let tid = tx.tid in
  let v = tx.tx_args.msg_value in
  let success_return =
    if Solc.Ver.(of_string_exn tseq.compiler_version >= v0_5_0) then
      {%string|(bool success_%{tid#Int},)|}
    else {%string|bool success_%{tid#Int}|}
  in
  if Value.is_integer_zero v then
    {%string|%{pad 4}%{success_return} = address(victim).call("");
%{pad 4}require(success_%{tid#Int}, "validator: unexpected receive revert");|}
  else
    {%string|%{pad 4}%{success_return} = address(victim).call%{value_modifier tseq tx}("");
%{pad 4}require(success_%{tid#Int}, "validator: unexpected receive revert");|}
;;

let normal_fn_content tseq (tx : Tx.t) (args : string) =
  let f = tx.fname in
  let v = tx.tx_args.msg_value in
  if Value.is_integer_zero v then {%string|%{pad 4}victim.%{f}(%{args});|}
  else {%string|%{pad 4}victim.%{f}%{value_modifier tseq tx}(%{args});|}
;;

module Expect_wrap = struct
  (** Use this wrapper if you are validating delayed safety queries. *)
  let emit content =
    {%string|%{pad 4}vm.expectEmit();
%{pad 4}emit smartest_check(false);
%{content}|}
  ;;

  (** Use this wrapper if you are validating non-DSC queries, like ASSERT, DZ or ERC20. *)
  let revert content = {%string|%{pad 4}vm.expectRevert("%{Vd_util.revert_name}");
%{content}|}

  let revert_su content = {%string|%{pad 4}vm.expectRevert("selfdestructed");
%{content}|}

  let main (tseq : Tseq.t) content =
    if tseq.kind = EL || tseq.kind = RE_EL then content
    else if tseq.delayed_safety then emit content
    else if tseq.kind = SU then revert_su content
    else revert content
  ;;
end

(** {b Returns} declaration contents, function call contents *)
let tx_argwrite_contents (tseq : Tseq.t) (tx : Tx.t) =
  let decl_strs, args_inside_paren = tx_make_args tx in
  let fn_call =
    if tx.fn_kind = Constr then constructor_content tseq tx args_inside_paren
    else if tx.fn_kind = Fallback then fallback_content tseq tx
    else if tx.fn_kind = Receive then receive_content tseq tx
    else normal_fn_content tseq tx args_inside_paren
  in
  let fn_call =
    if tx.tid = List.length tseq.tseq - 1 then Expect_wrap.main tseq fn_call else fn_call
  in
  (decl_strs, fn_call)
;;

let tx_content prepare_content decl_content fncall_content =
  {%string|%{prepare_content}
%{decl_content}
%{fncall_content}
|}
;;

let setting_scenario_for_tx (tseq : Tseq.t) (i : int) (tx : Tx.t) =
  let prepare = tx_prepare_template tx in
  let decl, fncall = tx_argwrite_contents tseq tx in
  {%string|function setScenario%{i#Int}() public {
    emit TxSeqDepth(tx_count);
    %{prepare}
%{decl}
%{fncall}
    inc_tx();
  }|}
;;

let address_number_to_contract v c a =
  if v >= v0_6_0 then {%string|%{c}(payable(address(%{a})))|} else {%string|%{c}((address(%{a})))|}
;;

let setting_scenarios tseq =
  let sl = List.mapi (setting_scenario_for_tx tseq) tseq.tseq in
  String.concat "\n  " sl
;;

(** [test_template this_addr contract_name test_body] *)
let test_template (tseq : Tseq.t) test_body =
  let c = tseq.main_contract in
  let initial_balance = sv tseq.send_eth ^ "0" in
  let v = Solc.Ver.of_string_exn tseq.compiler_version in
  (* multiply 10, give sufficient ether *)
  {%string|%{header v}
  
// [INFO] original `this` address: %{sv tseq.this_address}

contract SmarTestTest is SmarTestTestUtil, DepthControl {
  uint constant public initialBalance = %{initial_balance};
  uint constant public initialVictimBalance = %{sv tseq.init_eth};
    
  // target contract
  %{c} public victim = %{address_number_to_contract v c (sv tseq.this_address)};
  // attack contract
  SmarTestTestAttack public attacker;
  
  address[] public untrusted;
  mapping(address => bool) public untrust_map;
  
  event smartest_check(bool safety_cond);
  event smartest_check2(uint attacker_balance, uint stolen_money);
  event TxSeqDepth(uint depth);
  event TestResult(bool falsified, string message);
  event DebugUint(uint value);
  event Invest_sum(int value);
  event DebugLog(string message);
  event DebugBytes32(bytes32 data);
  
  function deployCitizenTo(uint256 value, address where) internal {
    deployCodeTo("SmarTestTestCitizen", abi.encode(), value, where);
  }
  
  function deployAttackTo(uint256 value, address where) internal {
    require(untrust_map[where], "trusted address cannot be an attack contract");
    deployCodeTo("SmarTestTestAttack", abi.encode(victim, this), value, where);
  }
  
  function deployAttackProxyTo(bytes memory args, uint256 value, address where) internal {
    require(untrust_map[where], "trusted address cannot be an attack contract");
    deployCodeTo("SmarTestTestAttackProxy", args, value, where);
  }
  
  function isEtherLeaked() internal returns (bool) {
    uint s = 0;
    for (uint i = 0; i < untrusted.length; i += 1) {
      // Normally, this is nonsense because balances are very small wrt 2^256. 
      // We check the overflow since Forge is able to set balances arbitrarily high.
      require(s + untrusted[i].balance >= s, "isEtherLeaked : integer overflow");
      s += untrusted[i].balance;
    }
    emit DebugUint(s);
    emit Invest_sum(int(untrusted.length * initialBalance) - int(s));
    return s > untrusted.length * initialBalance;
  }
  
%{calldatas (List.length tseq.tseq)}
  
  function setScenarioInit() public {
    vm.recordLogs();
    
    %{init_balance_and_trusts tseq}
    
    %{init_attackers tseq}
  }
  
  %{setting_scenarios tseq}

  function testScenario() public {
%{test_body}
  }
}
|}
;;

let test_body_template (tseq : Tseq.t) : string =
  match tseq.kind with
  | EL | RE_EL -> {%string|%{pad 4}require(isEtherLeaked(), "failed to steal");|}
  | _ -> ""
;;

let get_testfile (tseq : Tseq.t) : string = test_template tseq (test_body_template tseq)

(*************************************************)
(* Reentrancy Attack                             *)
(*************************************************)

let attack_statement tseq tx_depth re_depth (reent_info : Reent_info.t) (reent_tseq : Tx.t list) =
  let t, r = (tx_depth, re_depth) in
  let o = reent_info.order + 1 in
  let fncalls = List.map (tx_argwrite_contents tseq) reent_tseq in
  let fncalls = List.map (fun (a, b) -> tx_content "" a b) fncalls in
  let fncalls = String.concat "\n" fncalls in
  {%string|    if (test.tx_count() == %{t#Int} && test.ord_count() == %{o#Int} && test.re_count() == %{r#Int}) {
      require(!success);
%{fncalls}
      success = true;
    }|}
;;

let attack_function_of_tseq (tseq : Tseq.t) =
  let l = tseq.tseq in
  let rec aux tx_depth re_depth acc (tx : Tx.t) : string list =
    let reent_info, reenters = (List.map fst tx.reenter, List.map snd tx.reenter) in
    let attack_statements =
      List.map2 (attack_statement tseq tx_depth re_depth) reent_info reenters
    in
    let acc = acc @ attack_statements in
    let reenter_txs = List.concat reenters in
    List.fold_left (aux tx_depth (re_depth + 1)) acc reenter_txs
  in
  let l = List.mapi (fun i tx -> aux i 1 [] tx) l in
  List.concat l
;;

let fallback_sig (v : Solc.Ver.t) = if v >= v0_6_0 then {|fallback()|} else {|function()|}

let simple_success_check_var_decl (v : Solc.Ver.t) =
  if v >= v0_5_0 then {|(bool success, )|} else {|bool success|}
;;

let get_attacker (tseq : Tseq.t) =
  let v = Solc.Ver.of_string_exn tseq.compiler_version in
  let attack_functions = attack_function_of_tseq tseq in
  let attack_functions = String.concat "\n\n" attack_functions in
  {%string|contract SmarTestTestAttack {
  event Entering(uint n);
  
  %{tseq.main_contract} victim;
  SmarTestTest test;
    
  constructor(%{tseq.main_contract} _victim, SmarTestTest _test) public {
    victim = _victim;
    test = _test;
  }
    
  function smarTestTestAttackMain() public payable {
    emit Entering(gasleft());
    test.inc_ord();
    test.inc_re();
    bool success = false;
        
%{attack_functions}
        
    test.dec_re();
  }
    
  %{fallback_sig v} external payable {
    emit Entering(gasleft());
    if (gasleft() > 2500) { // try to reenter only when we have sufficient gas
      smarTestTestAttackMain();
    }
  }
}

// this is just an experimental feature, currently not used
contract SmarTestTestAttackProxy {
  address owner;
  constructor(address _owner) public {
    owner = _owner;
  }
  %{fallback_sig v} external payable {
    %{simple_success_check_var_decl v} = owner.delegatecall(msg.data);
    require(success);
  }
}

contract SmarTestTestCitizen {
  %{fallback_sig v} external payable {}
}

|}
;;
