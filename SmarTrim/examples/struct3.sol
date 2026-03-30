contract Test{
  mapping(uint => Bet) bets;
  struct Bet {
    uint80 amount;
    uint8 modulo;
    mapping (uint=>uint) balances;
  }

  function test(uint commit) public {
    Bet storage bet = bets[commit];
    uint amount = bet.amount;
    assert(amount == 0); // should not raise alarm.
	  uint amount2 = bet.balances[0]; 
	  assert(amount2 == 0); // should not raise alarm.
  }
}
