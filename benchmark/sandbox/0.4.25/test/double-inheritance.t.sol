// SPDX-License-Identifier: UNLICENSED
  
pragma experimental ABIEncoderV2;

import "../src/0x627f.sol";

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

contract Counter {
  Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
  TokenBank t;
  
  function setUp() {
    vm.prank(address(0x123));
    t = new TokenBank();
    vm.deal(address(t), 1);
  }
  
  function test() {
    vm.prank(address(0x456));
    t.initTokenBank();
    t.giveOwnerMoney();
    require(address(0x456).balance == 0);
  }
}