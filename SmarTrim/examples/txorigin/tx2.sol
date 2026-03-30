pragma solidity ^0.4.24;

contract MyContract {

    address owner;

    function MyContract() public {
        owner = msg.sender;
    }

    modifier onlyOwner{
      require(tx.origin == owner); // <yes> <report> ACCESS_CONTROL
      _;
    }

    function sendTo(address receiver, uint amount) public onlyOwner {
        receiver.transfer(amount);
    }

}
