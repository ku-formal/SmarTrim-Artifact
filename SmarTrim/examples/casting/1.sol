contract Test{
  function test () public {
    uint aaa = uint(-1);
    assert (aaa == 2**256 -1);
  }
}
