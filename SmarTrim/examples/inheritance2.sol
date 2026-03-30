contract B {
    event bb(string bstr);
    // when the root contract is 'A', this function cannot be called. 
    function internalf () internal{
        emit bb("bb");
        
    }
    function outer () public{
        internalf(); 
    }
}

contract A is B{
    event aa(string astr);
    
    function internalf () internal{
        emit aa ("aa");
    }
}
