// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/suicidal.sol";

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

// [INFO] original `this` address: (Elem ("0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf", T_address))

contract Counter {
  Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
  
  address[] public untrusted;
  
  event smartest_check(bool safety_cond);
  event smartest_check2(uint attacker_balance, uint stolen_money);
  event TestResult(bool falsified, string message);
  event DebugUint(uint value);
  event DebugLog(string message);
  event DebugBytes32(bytes32 data);
  
  Suicidal s;
  
  function isContractDestroyed(address contractAddress) public returns (bool) {
    uint256 size;
    assembly {
      size := extcodesize(contractAddress)
    }
    emit DebugUint(size);
    return size == 0;
  }
  
  function beforeTestSetup(bytes4 testSelector) public pure 
  returns (bytes[] memory beforeTestCalldata) {
    if (testSelector == this.testScenario2.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario2.selector);
    }
  }
    
  function testScenario1() public {
    s = new Suicidal();
    s.suicidal();
  }
  
  function setScenario2() public {
    s = new Suicidal();
    s.suicidal();
  }
  
  function testScenario2() public {
    s.incX();
  }
}

