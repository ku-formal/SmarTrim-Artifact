
contract etherSinkhole{
    constructor() public{}
    function destroy() public{
        selfdestruct(msg.sender);
    }
}