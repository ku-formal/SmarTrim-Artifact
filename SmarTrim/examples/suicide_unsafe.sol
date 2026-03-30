contract Example {
    address owner;

    constructor () {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require (msg.sender == owner);
        _;
    }

    function setOwner (address newOwner) public {
        owner = newOwner; 
    }

    function exploit (address attacker) public onlyOwner {
	    selfdestruct(msg.sender);
    }
}
