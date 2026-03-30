contract D {
  uint public k;
  uint public n;
  address public sender;

  event log (uint a);
  event blog(bool bb);

  function delegatecallSetN(address _e, uint _n) public {
    emit log (n);

    bool b = _e.delegatecall(bytes4(keccak256("setN(uint256)")), _n); // D's storage is set, E is not modified 
    emit blog(b);
    
    emit log (n);
  }
}

contract E {
  uint public v;
  uint public c;
  address public sender;
  event log2(uint b);
  event log_addr( address);
  
  function setN(uint _n) public {
    c = _n;
    emit log2(c);
    sender = msg.sender;
    emit log_addr(msg.sender);
  }
}
