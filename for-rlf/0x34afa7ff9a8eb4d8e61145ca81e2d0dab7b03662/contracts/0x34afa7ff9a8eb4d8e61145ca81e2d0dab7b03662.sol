

contract Destroyable{
    
    function destroy() public{
        selfdestruct(address(this));
    }
}