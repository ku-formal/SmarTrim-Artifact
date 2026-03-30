pragma solidity ^0.4.25;

interface A {
    function f() public pure returns (uint);
}

interface B {
    function f() public pure returns (uint);
}

contract C {
    event UINT(uint);
    function g() public {
        A a = A(0x12345);
        B b = B(0x12345);
        uint c = b.f();
        emit UINT(c);
    }
}