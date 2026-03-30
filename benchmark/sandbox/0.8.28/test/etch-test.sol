// SPDX-License-Identifier: UNLICENSED

pragma solidity ^0.8.28;

import "../src/static-test.sol";
import {Test} from "forge-std/Test.sol";

contract A {
  uint x;
  constructor() {
    x = 100;
  }
  function a() external returns (uint) {
    x += 1;
    return x;
  }
}

contract B {
  uint public x;
  constructor(uint y) {
    x = y;
  }
}

contract Counter is Test {  
  A a;
  A b;
  B c;
  
  event Debug(uint u);
  
  function setUp() external {
    a = new A();
    b = A(address(0x777));
    vm.etch(address(b), address(a).code);
    address addr = address(0x999);
    deployCodeTo("B", abi.encode(422), addr);
    c = B(addr);
  }
  
  function testA() external {
    uint cc = b.a();
    emit Debug(cc);
  }
  
  function testB() external {
    emit Debug(c.x());
  }
}
