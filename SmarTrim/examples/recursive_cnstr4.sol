contract B{
  uint bbb;
  constructor () public {
	bbb = 8;
  }

}

contract Example is B {
  event log (uint n);

  constructor (uint n) Example(5) public {
    // Example (5) is ignored.
    emit log (n);
  }
}
