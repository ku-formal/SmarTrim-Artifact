contract Test{
  address owner = msg.sender;
  enum State {
    SETUP, OPEN, CLOSED, PLAYERS_WIN, OWNER_WIN
  }

  State public state;

  modifier inState(State expected) {
    require(state == expected);
    _;
  }

  modifier onlyOwner {
    require (msg.sender == owner); 
    _;
  }

  // expected := State.SETUP should not be considered as lines of the function body
  function setOwner (address _owner) public onlyOwner inState(State.SETUP)
  { 
    owner = _owner;
  }

  function withdraw ( ) public {
    msg.sender.transfer(1 ether);
  }
}
