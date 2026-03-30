contract B {
  event num(uint);
  uint bb;
  constructor (uint b) public {
    num(b);
	bb =b;
  }
}


contract A is B{
  uint aa = 10;
  constructor (uint a) public B(3) {
    num(a);
  }
}

contract Test is A{
  address owner = msg.sender;
  constructor () public A(5) B(8) {
    // print '8' => '5'
	// i.e., executed in the order of B(8) => A(5)
  }
}
