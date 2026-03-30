// SPDX-License-Identifier: UNLICENSED

pragma solidity ^0.8.28;

import {Test} from "forge-std/Test.sol";

struct Nmixx {
  uint lily;
  uint haewon;
}

contract NmixxReturns {
  function give() external pure returns (Nmixx memory nmixx) {
    nmixx = Nmixx({
      lily: 1017,
      haewon: 225
    });
  }
}

contract Counter is Test {  
  event TestResult(bool falsified, string message);
  event DebugUint(uint value);
  event DebugLog(string message);
  event DebugBytes32(bytes32 data);
  
  mapping(address => Nmixx) nmixxes;
  
  function beforeTestSetup(bytes4 testSelector) public pure 
  returns (bytes[] memory beforeTestCalldata) {
    if (testSelector == this.test0.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.set0.selector);
    }
  }
  
  function setUp() public {
    
  }
    
  function set0() public {
    Nmixx memory nmixx = nmixxes[msg.sender];
    nmixx.lily += 1017;
    nmixx.haewon += 225;
    emit DebugUint(nmixx.lily);
  }
  
  function test0() public {
    Nmixx memory nmixx = nmixxes[msg.sender];
    emit DebugUint(nmixx.lily);
    require(nmixx.lily == 0);
    require(nmixx.haewon == 0);
  }
  
  function test1() public {
    emit DebugBytes32(bytes32("abcdef"));
    NmixxReturns nmixxReturns = new NmixxReturns();
    Nmixx memory nmixx = nmixxReturns.give();
    require(nmixx.lily == 1017);
  }
}
