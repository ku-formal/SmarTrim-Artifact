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
  function getCode(string calldata) external returns (bytes memory);
  function etch(address who, bytes code) external;
}

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

contract Counter {  
  Vm vm = Vm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
  
  function asmcall(address target, uint256 value) public returns (bool success, bytes memory returndata) {
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
  }
  
  function deployCodeTo(string memory what, bytes memory args, uint256 value, address where) internal {
    bytes memory creationCode = vm.getCode(what);
    vm.etch(where, abi.encodePacked(creationCode, args));
    (bool success, bytes memory runtimeBytecode) = asmcall(where, value);
    require(success, "StdCheats deployCodeTo(string,bytes,uint256,address): Failed to create runtime bytecode.");
    vm.etch(where, runtimeBytecode);
  }
  
  B b;
  
  event Debug(uint u);
  
  function setUp() external {
    address addr = address(0x999);
    deployCodeTo("B", abi.encode(422), 0, addr);
    b = B(addr);
  }
  
  function testB() external {
    emit Debug(b.x());
  }
}
