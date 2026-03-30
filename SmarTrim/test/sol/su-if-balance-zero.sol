contract SuIfBalanceZero {
    uint like;
    function hello() external {
        require(address(this).balance == 0);
        selfdestruct(payable(msg.sender));
    }
    function addLike() external {
        like++;
    }
}