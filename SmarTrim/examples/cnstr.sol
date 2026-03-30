contract B {
    event log (uint);
    event log2 (string);
    uint decimal = 5;
    function B (uint _decimal){
        emit log2("B");
        // emit log (decimal);
        decimal = _decimal;
    }
}


contract A is B {
    uint tokenDecimal = 18;
    modifier onlyOwner {
        log2("onlyOwner");
        _;
    }
    function A () B(tokenDecimal) public onlyOwner {
       // "B" -> "onlyOwner" (i.e., cnstr call -> modifier call)
    }
    
}
