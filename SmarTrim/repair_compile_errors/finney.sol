contract Test {
  function test (uint a, uint b) public {
    require(1 finney * a > b);
    uint c = 1 finney * a;
  }
}
