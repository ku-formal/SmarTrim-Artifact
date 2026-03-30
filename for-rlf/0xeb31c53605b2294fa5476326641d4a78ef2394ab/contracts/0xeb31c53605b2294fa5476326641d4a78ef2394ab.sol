

contract ReverseBugBounty {
    address owner;

    function () payable {
        revert; 
    }

    function ReverseBugBounty(){
        owner = msg.sender;
    }
    
    function destroy(){
        selfdestruct(owner);
    }
}