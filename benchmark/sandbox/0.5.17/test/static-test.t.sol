// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/static-test.sol";

interface Vm {
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
}

contract Counter {
  Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
  
  address[] public untrusted;
  
  event smartest_check(bool safety_cond);
  event smartest_check2(uint attacker_balance, uint stolen_money);
  event TestResult(bool falsified, string message);
  event DebugUint(uint value);
  event DebugLog(string message);
  event DebugBytes32(bytes32 data);
  
  ModifierEntrancy m = new ModifierEntrancy();
  Attack a = new Attack();
  NoAttack n = new NoAttack();
  
  function setUp() public {
    
  }
    
  function testA() public {
    emit DebugUint(0);
    vm.expectRevert();
    a.attaque(address(m));
  }
  
  function testB() public {
    emit DebugUint(0);
    n.noattaque(address(m));
    emit DebugUint(m.tokenBalance(address(n)));
  }
}
