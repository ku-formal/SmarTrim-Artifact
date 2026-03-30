/* must pass when in exploit mode */
contract Test{
  function test (uint bbb) public {
    int aaa = ~(2*2);
    assert (aaa == 3);
  }
}
