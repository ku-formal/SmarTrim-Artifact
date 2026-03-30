
contract Test {
  function test (address to, uint a, uint b) public {
    require (to != 0x0);
    to.transfer(10);
  }
}
