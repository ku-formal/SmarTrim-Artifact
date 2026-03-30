contract B {
  event log (uint n);
  uint number;

  constructor (uint n) B(5) public {
    number = n+1;
  }
}

contract C  is B {

}
