contract B {
	address owner;

	modifier onlyOwner () {
		require (owner == msg.sender);
		_;

	}


}

contract A is B {
	address owner;

	function test () onlyOwner public {

	}
}
