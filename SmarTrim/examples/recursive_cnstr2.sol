contract Example {
  event log (uint n);

  constructor (uint n) Example(5) public {
    emit log (n);
  }
}

contract A is Example(8) { // allowed in 0.4.26 but not allowed in 0.5.12
   // for 0.4.26, Example (8) is called and Example (5) is ignored.
}
