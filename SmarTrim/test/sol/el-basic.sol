contract EL {
    function here() external {
        payable(msg.sender).transfer(6000);
    }
    function notHere() external {
        require(false);
        payable(msg.sender).transfer(6700);
    }
}

contract EL2 {
    function here() external {
        payable(msg.sender).transfer(6000);
    }
    function notHere() external {
        require(false);
        payable(msg.sender).transfer(6700);
    }
    function notHere2() external payable {
        payable(msg.sender).transfer(msg.value);
    }
}