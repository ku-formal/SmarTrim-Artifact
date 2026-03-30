interface Oracle {
    function getUniOutput () external view returns (uint);
}

contract Test {
  function test (Oracle o) public view {
    o.getUniOutput();
  }

  function test () public {
    msg.sender.call.value (1)("");
  }
}
