contract B{
    uint nb;
    
    constructor (uint n) public {
        nb+=1; // safe
    }
    
}

contract A{
    function testa () public{
        B obj = new B(3);
    }
}
