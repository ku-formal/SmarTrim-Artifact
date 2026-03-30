contract Test{

  function test(address rcv, uint amount) public {
    uint prev_this = address(this).balance;
    uint prev_rcv = address(rcv).balance;

    rcv.transfer(amount);

    assert (address(this).balance == prev_this - amount); // valid
    assert (address(rcv).balance == prev_rcv + amount); // valid
    assert (address(this).balance + address(rcv).balance == prev_this + prev_rcv); // valid
  }

  function test2(address rcv, uint amount) public {
    uint prev_this = address(this).balance;
    uint prev_rcv = address(rcv).balance;

    rcv.call.value(amount)("");

    assert (address(this).balance == prev_this - amount); // valid
    assert (address(rcv).balance == prev_rcv + amount); // valid
    assert (address(this).balance + address(rcv).balance == prev_this + prev_rcv); // valid
  }

  function test3(address rcv, uint amount) public {
    uint prev_this = address(this).balance;
    uint prev_rcv = address(rcv).balance;

    rcv.send(amount);

    assert (address(this).balance == prev_this - amount); // valid
    assert (address(rcv).balance == prev_rcv + amount); // valid
    assert (address(this).balance + address(rcv).balance == prev_this + prev_rcv); // valid
  }
}
