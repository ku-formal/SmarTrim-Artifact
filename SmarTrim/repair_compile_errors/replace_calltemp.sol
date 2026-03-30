contract Test {
  function test (uint a, uint b) public {
    if(!msg.sender.send(a * b)){
    // if(!msg.sender.send((a * b))){
      uint c = a+b;
    }
  }
}
