contract Test {
  function test (uint a, uint b) public {
    require (
	    a+b == 100);
  }
}
