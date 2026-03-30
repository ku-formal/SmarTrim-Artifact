contract Test {
  function nn () public returns(uint) {
    return 100;
  }

  function test (uint a, uint b, uint c) public returns(uint) {
    return c 
             -(uint)(a+b)  - 1;
  }
}
