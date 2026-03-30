contract B {
  function kill() public {
    selfdestruct(msg.sender);
  }
}

contract A {
  address owner=msg.sender;
}

contract Test is A,B {
  function kill2() public {
    selfdestruct(msg.sender);
  }
}
