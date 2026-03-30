


library SafeMath {

    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        if (a == 0) {
            return 0;
        }
        uint256 c = a * b;
        assert(c / a == b);
        return c;
    }

    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        
        uint256 c = a / b;
        
        return c;
    }

    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        assert(b <= a);
        return a - b;
    }
    
    function add(uint256 a, uint256 b) internal pure returns (uint256) {

        uint256 c = a + b;
        assert(c >= a);
        return c;
    }
}


contract ERC223Interface {
    uint public totalSupply;
    function balanceOf(address who) public view returns (uint);
    function transfer(address to, uint value) public;
    function transfer(address to, uint value, bytes memory data) public;
    event Transfer(address indexed from, address indexed to, uint value, bytes data);
}



interface ERC223ReceivingContract {
    
    function tokenFallback( address from, uint value, bytes  data ) external;
}



contract Ownership {

    address public owner;
    event LogOwnershipTransferred(address indexed oldOwner, address indexed newOwner);


    constructor() public {
        owner = msg.sender;
    }

    modifier onlyOwner {
        require(msg.sender == owner, "Only owner is allowed");
        _;
    }

    
    function transferOwnership(address _newOwner)
        public
        
    {
        require(_newOwner != address(0), "Zero address not allowed");
        address oldOwner = owner;
        owner = _newOwner;
        emit LogOwnershipTransferred(oldOwner, _newOwner);
    }

}


contract Freezable is Ownership {

    bool public emergencyFreeze;
    mapping (address => bool) public frozen;

    event LogFreezed(address indexed target, bool indexed freezeStatus);
    event LogEmergencyFreezed(bool emergencyFreezeStatus);

    modifier unfreezed(address _account) {
        require(!frozen[_account], "Account is unfreezed");
        _;
    }

    modifier noEmergencyFreeze() {
        require(!emergencyFreeze, "Account is freezed");
        _;
    }

    
    function freezeAccount (address _target, bool _freeze)
        public
        onlyOwner
    {
        require(_target != address(0), "Zero address not allowed");
        frozen[_target] = _freeze;
        emit LogFreezed(_target, _freeze);
    }

   
    function emergencyFreezeAllAccounts (bool _freeze)
        public
        onlyOwner
    {
        emergencyFreeze = _freeze;
        emit LogEmergencyFreezed(_freeze);
    }
}



contract StandardToken is ERC223Interface, Freezable {

    using SafeMath for uint;

    string public name;
    string public symbol;
    uint public decimals;
    uint public totalSupply;
    uint public currentSupply;

    mapping (address => uint) internal balances;
    mapping (address => mapping (address => uint) ) private  allowed;

    event Approval(address indexed tokenOwner, address indexed spender, uint tokens);

    constructor () public {
        name = 'CNEXCHANGE';
        symbol = 'CNEX';
        decimals = 8;
        totalSupply = 400000000 * ( 10 ** decimals ); 
    }

    
    function transfer(address _to, uint _value)
        public
        unfreezed(_to)
        unfreezed(msg.sender)
        noEmergencyFreeze()
    {
        bytes memory _data;
        _transfer223(msg.sender, _to, _value, _data);
    }

    
    function transfer(address _to, uint _value, bytes memory _data)
        public
        unfreezed(_to)
        unfreezed(msg.sender)
        noEmergencyFreeze()
    {
        _transfer223(msg.sender, _to, _value, _data);
    }

    
    function isContract( address _addr )
        private
        view
        returns (bool)
    {
        uint length;
        assembly { length := extcodesize(_addr) }
        return (length > 0);
    }

    
    function approve(address _spender, uint _value)
        public
        unfreezed(_spender)
        unfreezed(msg.sender)
        noEmergencyFreeze()
        returns (bool success)
    {
        require((_value == 0) || (allowed[msg.sender][_spender] == 0), "Approval needs to be 0 first");
        require(_spender != msg.sender, "Can not approve to self");
        allowed[msg.sender][_spender] = _value;
        emit Approval(msg.sender, _spender, _value);
        return true;
    }

    
    function increaseApproval(address _spender, uint _addedValue)
        public
        unfreezed(_spender)
        unfreezed(msg.sender)
        noEmergencyFreeze()
        returns (bool success)
    {
        require(_spender != msg.sender, "Can not approve to self");
        allowed[msg.sender][_spender] = allowed[msg.sender][_spender].add(_addedValue);
        emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
        return true;
    }

    
    function decreaseApproval(address _spender, uint _subtractedValue)
        public
        unfreezed(_spender)
        unfreezed(msg.sender)
        noEmergencyFreeze()
        returns (bool success)
    {
        require(_spender != msg.sender, "Can not approve to self");
        uint oldAllowance = allowed[msg.sender][_spender];
        if (_subtractedValue > oldAllowance) {
            allowed[msg.sender][_spender] = 0;
        } else {
            allowed[msg.sender][_spender] = oldAllowance.sub(_subtractedValue);
        }
        emit Approval(msg.sender, _spender, allowed[msg.sender][_spender]);
        return true;
    }

    
    function transferFrom(address _from, address _to, uint _value)
        public
        unfreezed(_to)
        unfreezed(msg.sender)
        noEmergencyFreeze()
        returns (bool success)
    {
        require(_value <= allowed[_from][msg.sender], "Insufficient allowance");
        allowed[_from][msg.sender] = allowed[_from][msg.sender].sub(_value);
        bytes memory _data;
        _transfer223(_from, _to, _value, _data);
        return true;
    }

    
    function transferFrom(address _from, address _to, uint _value, bytes memory _data)
        public
        unfreezed(_to)
        unfreezed(msg.sender)
        noEmergencyFreeze()
        returns (bool success)
    {
        require(_value <= allowed[_from][msg.sender], "Insufficient allowance");
        allowed[_from][msg.sender] = allowed[_from][msg.sender].sub(_value);
        _transfer223(_from, _to, _value, _data);
        return true;
    }


    
    function burn(uint256 _value)
        public
        unfreezed(msg.sender)
        noEmergencyFreeze()
        onlyOwner
        returns (bool success)
    {
        require(balances[msg.sender] >= _value, "Insufficient balance");
        balances[msg.sender] = balances[msg.sender].sub(_value);
        totalSupply = totalSupply.sub(_value);
        currentSupply = currentSupply.sub(_value);
        bytes memory _data;
        emit Transfer(msg.sender, address(0), _value, _data);
        return true;
    }

  
    
    function balanceOf(address _tokenOwner) public view returns (uint) {
        return balances[_tokenOwner];
    }

    
    function allowance(address _tokenOwner, address _spender) public view returns (uint) {
        return allowed[_tokenOwner][_spender];
    }

    
    function transferAnyERC20Token(address _tokenAddress, uint _value)
        public
        onlyOwner
    {
        ERC223Interface(_tokenAddress).transfer(owner, _value);
    }

    
    function _transfer223(address _from, address _to, uint _value, bytes memory _data)
        private
    {
        require(_to != address(0), "Zero address not allowed");
        require(balances[_from] >= _value, "Insufficient balance");
        uint balBeforeTransfer = balances[_from].add(balances[_to]);
        balances[_from] = balances[_from].sub(_value);
        balances[_to] = balances[_to].add(_value);
        if (isContract(_to)) {
            ERC223ReceivingContract receiver = ERC223ReceivingContract(_to);
            receiver.tokenFallback(msg.sender, _value, _data);
        }
        uint balAfterTransfer = balances[_from].add(balances[_to]);
        assert(balBeforeTransfer == balAfterTransfer);
        emit Transfer(_from, _to, _value, _data); 
    }

}


contract CNEXToken is StandardToken {

    uint public icoFunds;
    uint public consumerProtectionFund;
    uint public ecoSystemDevelopmentAndOperationFund;
    uint public teamAndFounderFund;

    bool public consumerProtectionFundAllocated = false;
    bool public ecoSystemDevelopmentAndOperationFundAllocated = false;
    bool public teamAndFounderFundAllocated = false;

    uint public tokenDeploymentTime;

    constructor() public StandardToken(){
        owner = msg.sender;
        icoFunds = 200000000 * (10 ** decimals); 
        consumerProtectionFund = 60000000 * (10 ** decimals); 
        ecoSystemDevelopmentAndOperationFund = 100000000 * (10 ** decimals); 
        teamAndFounderFund = 40000000 * (10 ** decimals); 
        tokenDeploymentTime = now;
        _mint(msg.sender, icoFunds);
    }
    
    
    function allocateConsumerProtectionFund()
        public
        onlyOwner
    {
        require(!consumerProtectionFundAllocated, "Already allocated");
        consumerProtectionFundAllocated = true;
        _mint(owner, consumerProtectionFund);
    }

    
    function allocateEcoSystemDevelopmentAndOperationFund()
        public
        onlyOwner
    {
        require(!ecoSystemDevelopmentAndOperationFundAllocated, "Already allocated");
        ecoSystemDevelopmentAndOperationFundAllocated = true;
        _mint(owner, ecoSystemDevelopmentAndOperationFund);
    }
    
    
    function allocateTeamAndFounderFund()
        public
        onlyOwner
    {
        require(!teamAndFounderFundAllocated, "Already allocated");
        require(now > tokenDeploymentTime + 365 days, "Vesting period not over yet");
        teamAndFounderFundAllocated = true;
        _mint(owner, teamAndFounderFund);
    }

    
    function kill(uint _code)
        public
        onlyOwner
    {
        require(_code == 1234567890, "Invalid code");
        selfdestruct(address(uint160(owner))); 
    }

    
    function _mint(address _to, uint _value)
        private
        onlyOwner
    {
        require(currentSupply.add(_value) <= totalSupply, "Exceeds total supply");
        balances[_to] = balances[_to].add(_value);
        currentSupply = currentSupply.add(_value);
        bytes memory _data;
        emit Transfer(address(0), _to, _value, _data);
    }
    
}
