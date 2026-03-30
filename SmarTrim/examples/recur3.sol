contract Example {
  function f (uint a) public {
	if (a==0) return;
	else g(a);
  }

  function g (uint b) public {
	f(b-1);
  }
}
