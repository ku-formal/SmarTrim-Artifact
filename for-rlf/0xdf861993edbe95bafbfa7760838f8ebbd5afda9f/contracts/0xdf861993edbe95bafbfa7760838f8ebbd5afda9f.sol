
contract Nobody {
    function die() public {
        selfdestruct(msg.sender);
    }
}