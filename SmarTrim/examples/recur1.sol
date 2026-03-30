contract Example {
  function fact (uint n) public returns (uint) {
	if (n==1) return 1;
	else fact (n-1);
  }
  
  function test () public {
	uint n =fact (5);
  }
}
