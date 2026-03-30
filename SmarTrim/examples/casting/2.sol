/* must pass when in exploit mode */
contract Test{
  function test (uint bbb) public {
    uint aaa = ~(2**bbb);
    assert (aaa == 2**256 -1);
  }
}
