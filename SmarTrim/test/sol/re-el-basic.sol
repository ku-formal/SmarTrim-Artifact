contract RE {
    mapping(address=>uint) balance;
    function put() external payable {
        balance[msg.sender] += msg.value;
    }
    function withdraw() external {
        uint v = balance[msg.sender];
        (bool succ,) = msg.sender.call{value: v}("");
        if (succ) {
            balance[msg.sender] = 0;
        }
    }
}

contract RESafe {
    mapping(address=>uint) balance;
    function put() external payable {
        balance[msg.sender] += msg.value;
    }
    function withdraw() external {
        uint v = balance[msg.sender];
        balance[msg.sender] = 0;
        (bool succ,) = msg.sender.call{value: v}("");
        require(succ);
    }
}

contract IntfLike {
    function viewOnly() external view {}
    function nonViewOnly() external {}
}

contract REIntf {
    mapping(address=>uint) balance;
    function put() external payable {
        balance[msg.sender] += msg.value;
    }
    function withdraw() external {
        uint v = balance[msg.sender];
        payable(msg.sender).transfer(v);
        IntfLike(msg.sender).nonViewOnly();
        balance[msg.sender] = 0;
    }
}

contract REIntf2 {
    mapping(address=>uint) balance;
    function put() external payable {
        balance[msg.sender] += msg.value;
    }
    function withdraw() external {
        uint v = balance[msg.sender];
        IntfLike(msg.sender).nonViewOnly();
        payable(msg.sender).transfer(v);
        balance[msg.sender] = 0;
    }
}

contract REIntfSafe {
    mapping(address=>uint) balance;
    function put() external payable {
        balance[msg.sender] += msg.value;
    }
    function withdraw() external {
        uint v = balance[msg.sender];
        payable(msg.sender).transfer(v);
        IntfLike(msg.sender).viewOnly();
        balance[msg.sender] = 0;
    }
}