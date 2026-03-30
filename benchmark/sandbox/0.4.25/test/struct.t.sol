// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/0x7b36.sol";

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
  function getCode(string calldata) external returns (bytes memory);
  function etch(address who, bytes code) external;
}

contract Tester {
  Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
  W_WALLET w;
  Log l;
  
  function setUp() {
    vm.prank(address(0x123));
    l = new Log();
    w = new W_WALLET(address(l));
  }
  
  function test() {
    vm.prank(address(0x456));
    vm.deal(address(0x456), 200);
    vm.warp(500);
    w.Put.value(100)(700);
    
    vm.prank(address(0x456));
    w.Collect(100);
  }
}