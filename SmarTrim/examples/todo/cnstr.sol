contract C {
  uint n;
  constructor (uint _n) public {
    require (_n >0);
    n = _n;
  }
}

contract B is C {
  constructor () public {

  }
}

contract A is B {
  /* inheritance order: A -> B -> C */
  /* constructors should executed in the order of C -> B -> A */
  constructor (uint _n) public C(_n) B() {
    assert (false);
  }
}
