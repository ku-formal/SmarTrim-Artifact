contract Test {
  function test (uint n) public {
    // require (n==10 || n<10);

    test2(n);
    test2(n);
    test2(n);
    test2(n);

    // assert (false);
  }

  function test2 (uint n) internal {
    if (n>1) { }
    if (n>1) { }
    if (n>1) { }
    if (n>1) { }

  }

  function test3 (uint n) internal {
    require (n>1);
    require (n>1);
    require (n>1);
  }
}
