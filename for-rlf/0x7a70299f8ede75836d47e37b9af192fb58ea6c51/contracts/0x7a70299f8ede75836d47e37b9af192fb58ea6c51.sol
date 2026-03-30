
contract GetsBurned {
    function () payable public {
    }

    function BurnMe() public {
        
        selfdestruct(address(this));
    }
}