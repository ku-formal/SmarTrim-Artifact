// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Vm} from "forge-std/Vm.sol";
import {stdError} from "forge-std/StdError.sol";

contract C {
    receive() payable external {}
}

contract F {
    receive() payable external {
        revert();
    }
    
    function giveEtherToHere() payable external {}
}

contract CounterTest is Test {
    address payable a;
    address payable b;
    C c;
    F f;

    function setUp() public {
        a = payable(address(0x123));
        b = payable(address(0x456));
        
        vm.deal(a, 1 ether);
        vm.deal(b, 10 ether);
        c = new C();
        f = new F();
    }
    
    function test_addressCallHigherThanBalance() public {
        vm.prank(a);
        (bool succ, ) = b.call{value : 5 ether}(""); // EVMError: OutOfFunds
        require(succ);
    }
    
    function test_addressSendHigherThanBalance() public {
        vm.prank(a);
        bool succ = b.send(5 ether); // EVMError: OutOfFunds
        require(succ);
    }
    
    function test_addressTransferHigherThanBalance() public {
        vm.prank(a);
        b.transfer(5 ether); // EVMError: OutOfFunds
    }
    
    function test_contractCallRevert() public {
        vm.prank(a);
        vm.expectRevert();
        (bool succ, ) = address(f).call{value : 0.5 ether}("");
        require(succ);
    }
    
    function test_contractSendRevert() public {
        vm.prank(a);
        vm.expectRevert();
        bool succ = payable(f).send(0.5 ether);
        require(succ);
    }
    
    function test_contractTransferRevert() public {
        vm.prank(a);
        vm.expectRevert();
        payable(f).transfer(0.5 ether);
    }
}
