contract Test{
  mapping(address=>Reading) public requestReading;

  struct Reading {
	uint256 timestamp;
	uint256 value;
	string zip;
  }

  function test () public {
    requestReading[msg.sender] = Reading (now, 3 ,"");
	assert (false);
  }
}
