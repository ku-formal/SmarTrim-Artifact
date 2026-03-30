contract Ownable {
    address private _owner;

    event OwnershipTransferred(address indexed previousOwner, address indexed newOwner);

    /**
     * @dev The Ownable constructor sets the original `owner` of the contract to the sender
     * account.
     */
    constructor (address newOwner) public {
        _owner = newOwner;
        emit OwnershipTransferred(address(0), _owner);
    }

    /**
     * @return the address of the owner.
     */
    function owner() public view returns (address) {
        return _owner;
    }

    /**
     * @dev Throws if called by any account other than the owner.
     */
    modifier onlyOwner() {
        require(!isOwner(), "Ownable: caller is not the owner"); // ORIGINAL: require(isOwner(), "Ownable: caller is not the owner");
        _;
    }

    function isOwner() public view returns (bool) {
        return msg.sender == _owner;
    }

}

contract Test is Ownable {
  constructor () public Ownable(msg.sender){}
  function kill(
  
  
  )
  public
  returns(uint num)  
  { 
    selfdestruct(msg.sender);
  }


  function kill2()
  public {
    selfdestruct(msg.sender);
  }

}
