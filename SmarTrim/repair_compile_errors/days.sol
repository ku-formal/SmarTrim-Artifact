
contract Test {
  function test(uint start) public {
    require (now > start + (3*365+1) * 1 days);
  }
}
