//pragma experimental SMTChecker;
  
contract Test{
  mapping (address => uint) balance;

  constructor () public {
    balance[msg.sender] = 100;
  }

  function transfer (address to, uint value) public {
    require (balance[msg.sender] >= value);
    balance[msg.sender] -= value;
    balance[to] += value;
  }
}
