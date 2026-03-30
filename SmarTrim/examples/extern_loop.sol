// pragma solidity >=0.8.0;

interface Unknown {
    function run() external;
}

contract Mutex {
    uint x;
    bool lock;
    mapping(address=>uint) balances;

    Unknown unknown;

    constructor(Unknown _u) {
        require(address(_u) != address(0));
        unknown = _u;
    }

    modifier mutex {
        require(!lock);
        lock = true;
        _;
        lock = false;
    }

    function set(uint _x) public {
        x = _x;
    }
    
    function run2() public {
        // balances[msg.sender]++;
    	unknown.run();
    }

    function run3 () public {
    	for(uint i=0; i<10; i++) {
          // bool res = lock == false;
	  msg.sender.call.value(1)(""); 
	}
    }
}
