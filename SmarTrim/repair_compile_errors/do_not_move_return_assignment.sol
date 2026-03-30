contract Test {
  function test (bool b) public returns(string) {
    if(b){
      msg.sender.call.value(1)("");
    }
    else {
      return "Test";
    }
  }

  function test2 (bool b) public returns(string) {
    msg.sender.call.value(1)("");
    return "Test";
  }
}
