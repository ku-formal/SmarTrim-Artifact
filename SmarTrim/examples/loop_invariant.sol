contract B {
	function btest(uint n) public {
		uint j=0;
		for (uint i=0; i<n; i++){
			j++;
		}
	}
}

contract A {
	function atest (uint n) public {
		uint j=0;
		for (uint i=0; i<n; i++){
			j++;
		}
	}

	function call (uint m) public {
		B b = new B();
		b.btest (m);
	}
}
