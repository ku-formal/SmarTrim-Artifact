contract Test {
  uint num = 10;
  address owner = msg.sender;

  function test1 () public {
    require(owner == msg.sender);
    test2();
    owner.transfer(this.balance);
  }

  function test2 () private {
    while(true){
    }
    owner = msg.sender;
  }
}
