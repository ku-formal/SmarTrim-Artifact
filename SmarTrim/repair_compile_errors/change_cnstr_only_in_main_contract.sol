contract A {
  address owner = msg.sender;

  constructor () public {

  }

  function setOwner (address newOwner) public {
    owner = msg.sender;
  }

  function kill() public {
    selfdestruct(msg.sender);
  }

}

contract Test is A {
}
