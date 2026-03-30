pragma solidity >=0.8.0;
pragma experimental SMTChecker;

interface Unknown {
    function run() external;
    function run2() external view;

}

contract Mutex {
    uint n;
    bool lock;

    Unknown immutable unknown;

    constructor(Unknown _u) {
        require(address(_u) != address(0));
        unknown = _u;
	n = 0;
    }

    modifier mutex {
        require(!lock);
        lock = true;
        _;
        lock = false;
    }


    function n10 (Unknown unknown) public mutex {
	n = 10;
        unknown.run();
        assert (n == 10);
    }

    function n5 () public mutex {
      n = 5;
    }
}
