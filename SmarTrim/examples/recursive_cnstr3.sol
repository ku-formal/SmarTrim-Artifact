contract Example {
  event log (uint n);

  constructor (uint n) Example(5) public {
    emit log (n);
  }
}

contract A is Example {
   // Example (5) is called.
}
