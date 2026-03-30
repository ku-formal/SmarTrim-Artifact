contract Token {
  address owner = msg.sender;
  uint totalSupply = 0;
  mapping(address=>uint) balance;

  modifier onlyOwner {
    require(owner == msg.sender);
    _;
  }

  function allocateTokens (address[] to, uint[] values) public onlyOwner {
    for (uint i = 0; i < to.length; i++) {
      require (totalSupply + values[i] >= totalSupply); // FIX ??
      balance[to[i]] += values[i];
      totalSupply += values[i];
    }
  }

  function transfer(address to, uint value) public {
    require(balance[msg.sender] >= value);
    balance[msg.sender] -= value;
    balance[to] += value;
  }

  function two_balance_sum_less_than_or_equal_to_totalSupply (address x, address y) public {
    require(x != y);
    assert(balance[x] + balance[y] <= totalSupply);
  }
}
