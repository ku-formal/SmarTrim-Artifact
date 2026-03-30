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

    function () external payable {
    }

    function exploit (address attacker, uint value) public onlyOwner {
        attacker.transfer (value);
    }
}
