pragma solidity ^0.8.18;

interface Token {
    function buy() external payable;
}

contract ELSemiExtcall {
    function vulnBuy(address c) external payable {
        Token(c).buy{value: 1 ether}();
    }
}

contract NoELSemiExtcall {
    function novulnBuy(address c) external payable {
        require(c == address(0));
        Token(c).buy{value: 1 ether}();
    }
}

///

contract AccessControl {
    function owner() external pure returns (address) {
        return address(0x123);
    }
}

contract AccessControlled { // not vulnerable
    AccessControl ac;
    
    constructor(address ac_) {
        ac = AccessControl(ac_);
    }
    
    function safe() payable external {
        require(msg.sender == ac.owner());
        payable(msg.sender).transfer(address(this).balance);
    }
}