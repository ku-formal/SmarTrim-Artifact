contract Test3 {
  struct Str3 {
    address addr;
    uint amount;
  } 
}

contract Test2 {

  struct Str  {
    address addr;
    uint amount;
  }
}


contract Test is Test3 {
  struct Str {
    address addr;
    uint amount;
  }

  struct Str2 {

    mapping (uint => Str) funders;
    address addr2;
  }

  Str2 str;

  function test (uint n) public {
    Str memory s1 = Str (address(0x1), n);
    Str memory s2 = s1;
    assert (s1.amount == s2.amount); // proven
  }

  function test2 (uint n) public {
    Str memory s1 = Str (address(0x1), n);
    Str memory s2 = s1;
    assert (s1.amount == s2.amount); // proven
  }

  function test3 (uint n) public {
    Test2.Str memory s1 = Test2.Str (address(0x1), n);
    Test2.Str memory s2 = s1;
    assert (s1.amount == s2.amount); // proven
  }

  function test4 () public {
    str = Str2 (address(0x2));

    Str2 storage s1 = str;
    // s1.funders[0].amount; // 0

    assert (s1.funders[0].amount == 0); //proven

    s1.funders[0] = Str (address(0x1), 99);

    assert (s1.funders[0].amount == 99); //proven
    // amount[(funders[0])[s1]]
  }

  function test5 (uint n) public {
    Str3 memory s1 = Str3 (address(0x1), n);
    Str3 memory s2 = s1;
    assert (s1.amount == s2.amount); // proven
  }
}
