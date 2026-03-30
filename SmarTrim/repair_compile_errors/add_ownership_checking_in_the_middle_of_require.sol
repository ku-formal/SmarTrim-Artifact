contract Test {
  address owner = msg.sender;

  function test (uint a) public 
  {
    require (
	    a>=10
    );
    selfdestruct(msg.sender);
  }

}
