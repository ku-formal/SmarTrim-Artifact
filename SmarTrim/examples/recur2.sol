contract Example {
  function f (uint n1) public {
	if (n1==1) return;
	else g(n1);
  }

  function g (uint n2) public {
	h(n2);
  }

  function h (uint n3) public {
	f(n3);
  }
}
