contract Suicidal {
    uint public x;
    constructor() {
        x = 15;
    }
    
    function suicidal() public returns (uint) {
        x += 1;
        selfdestruct(0x0);
        x += 1;
    }
    
    function incX() public {
        x += 1;
    }
}