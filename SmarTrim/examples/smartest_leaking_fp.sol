contract Test {
  mapping(address => uint) balance;

  function Test () public {
    balance[msg.sender] = 100;
  }

  function transfer(address from, address to, uint value) public {
    require (balance[from] >= value);
    require (balance[to] + value >= balance[to]);
    balance[from] -= value;
    balance[to] += value;
  }

  function () payable {
    require (balance[msg.sender] + msg.value >= balance[msg.sender]);
    balance[msg.sender] += msg.value;
  }

  function sell(uint amount, uint sellPrice) public {
    //transfer(msg.sender, this, amount);
    require(balance[msg.sender] >= amount);
    balance[msg.sender] -= amount;
    msg.sender.transfer (amount);
  }
}
