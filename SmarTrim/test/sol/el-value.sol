interface I {
    function f() external payable returns (bool);
}

contract ELvalue {
    function here() external {
        I(msg.sender).f.value(7460)();
    }
}

///
