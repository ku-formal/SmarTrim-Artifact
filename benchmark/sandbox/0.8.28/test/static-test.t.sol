// SPDX-License-Identifier: UNLICENSED

pragma solidity ^0.8.28;

import "../src/static-test.sol";
import {Test} from "forge-std/Test.sol";

contract Counter is Test {  
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
