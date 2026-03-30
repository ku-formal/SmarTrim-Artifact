




contract SafeMath {

    
    
    
    
    function mul(uint256 a, uint256 b) internal  returns (uint256) {
        uint256 c = a * b;
        assert(a == 0 || c / a == b);
        return c;
    }

    
    
    
    
    function div(uint256 a, uint256 b) internal  returns (uint256) {
        assert(b > 0);
        uint256 c = a / b;
        return c;
    }

    
    
    
    function sub(uint256 a, uint256 b) internal   returns (uint256) {
        assert(b <= a);
        return a - b;
    }

    
    
    
    function add(uint256 a, uint256 b) internal  returns (uint256) {
        uint256 c = a + b;
        assert(c >= a);
        return c;
    }

    
    
    
    function pow( uint256 a , uint8 b ) internal returns ( uint256 ){
        uint256 c;
        c = a ** b;
        return c;
    }
}

contract owned {

    bool public OwnerDefined = false;
    address public owner;
    event OwnerEvents(address _addr, uint8 action);

    
    function owned()
        internal
    {
        require(OwnerDefined == false);
        owner = msg.sender;
        OwnerDefined = true;
        OwnerEvents(msg.sender,1);
    }

    
    
    function transferOwnership( address newOwner )
        external
    {
        require(msg.sender == owner);
        require(newOwner != address(0));
        owner = newOwner;
        OwnerEvents(msg.sender,2);
    }
}

contract ERC20Token is owned, SafeMath{

    
    bool public tokenState;
    string public name = "DropDeck";
    string public symbol = "DDD";
    uint256 public decimals = 8;
    uint256 public totalSupply = 380000000000000000;
    uint256 public blocktime;
    address public ico;

    mapping(address => uint256) balances;
    mapping (address => mapping (address => uint256)) allowed;

    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed _owner, address indexed _spender, uint256 _value);

    
    
    
    
    
    function init( uint256 _blocktime,address _ico)
        external
    returns ( bool ){
        require(tokenState == false);
        owned;
        tokenState = true;
        balances[msg.sender] = totalSupply;
        blocktime = _blocktime;
        ico = _ico;
        return true;
    }

    
    
    
    function transfer(address _to, uint256 _value)
        public
    returns ( bool ) {
        require(tokenState == true);
        require(_to != address(0));
        require(_value <= balances[msg.sender]);
        require(block.number >= blocktime);
        balances[msg.sender] = sub(balances[msg.sender],_value);
        balances[_to] = add(balances[_to],_value);
        Transfer(msg.sender, _to, _value);
        return true;
    }
    
    
    
    
    function transferFrom(address _from, address _to, uint256 _value)
        public
    {
        require(_to != address(0));
        require(_value <= balances[_from]);
        require(_value <= allowed[_from][msg.sender]);

        balances[_from] = sub(balances[_from],_value);
        balances[_to] = add(balances[_to],_value);
        allowed[_from][msg.sender] = sub(allowed[_from][msg.sender],_value);
        Transfer(_from, _to, _value);

    }

    
    
    
    function transferICO(address _to, uint256 _value)
        public
    returns ( bool ) {
        require(tokenState == true);
        require(_to != address(0));
        require(_value <= balances[owner]);
        require(ico == msg.sender);
        balances[owner] = sub(balances[owner],_value);
        balances[_to] = add(balances[_to],_value);
        Transfer(msg.sender, _to, _value);
        return true;
    }

    
    
    function balanceOf(address _owner)
        external
        constant
    returns (uint256) {
        require(tokenState == true);
        return balances[_owner];
    }

    
    
    
    function approve(address _spender, uint256 _value)
        external
    returns (bool success) {
        require(tokenState == true);
        require(_spender != address(0));
        require(msg.sender == owner);
        allowed[msg.sender][_spender] = mul(_value, 100000000);
        Approval(msg.sender, _spender, _value);
        return true;
    }

    
    
    
    function allowance(address _owner, address _spender)
        external
        constant
    returns (uint256 remaining) {
        require(tokenState == true);
        return allowed[_owner][_spender];
    }
}
contract tokenContract is ERC20Token{

}

contract DDDico is SafeMath {

    tokenContract token;

    bool public state;

    address public wallet;
    address public tokenAddress;
    address public owner;

    uint256 public weiRaised;
    uint256 public hardCap;
    uint256 public tokenSale;
    uint256 public tokenLeft;
    uint256 public applicableRate;
    uint256 weiAmount;
    uint256 tok;

    uint256 public block0 = 4644650;
    uint256 public block1 = 4644890;
    uint256 public block2 = 4650650;
    uint256 public block3 = 4690970;
    uint256 public block4 = 4731290;
    uint256 public block5 = 4771610;
    uint256 public block6 = 4811930;

    event TokenPurchase(address indexed purchaser, address indexed beneficiary, uint256 value, uint256 amount);

    
    
    
    
    function DDDico( address _wallet, address _token , uint256 _hardCap, uint256 _tokenSale ) {
        require(_wallet != address(0));
        state = true;
        owner = msg.sender;
        wallet = _wallet;
        tokenAddress = _token;
        token = tokenContract(tokenAddress);
        hardCap = mul(_hardCap,pow(10,16));
        tokenSale = mul(_tokenSale,pow(10,8));
        tokenLeft = tokenSale;
    }

    
    function () payable {
        buyTokens();
    }

    
    function buyTokens() public payable {
        require(validPurchase());
        weiAmount               = 0;
        tok                     = 0;
        weiAmount               = msg.value;
        tok                     = div(mul(weiAmount,fetchRate()),pow(10,16));
        weiRaised               = add(weiRaised,weiAmount);
        tokenLeft               = sub(tokenLeft,tok);
        token.transferICO(msg.sender,tok);
        TokenPurchase(msg.sender, msg.sender, weiAmount, tok);
        forwardFunds();
    }

    
    function forwardFunds() internal {
        wallet.transfer(msg.value);
    }

    
    function validPurchase() internal constant returns (bool) {
        bool withinPeriod = block.number >= block0 && block.number <= block6;
        bool nonZeroPurchase = msg.value != 0;
        bool cap = weiRaised <= hardCap;
        return withinPeriod && nonZeroPurchase && cap;
    }

    
    function fetchRate() constant returns (uint256){
        if( block0 <= block.number && block1 > block.number ){
            applicableRate = 18700000000;
            return applicableRate;
        }
        if ( block1 <= block.number && block2 > block.number ){
            applicableRate = 16700000000;
            return applicableRate;
        }
        if ( block2 <= block.number && block3 > block.number ){
            applicableRate = 15000000000;
            return applicableRate;
        }
        if ( block3 <= block.number && block4 > block.number ){
            applicableRate = 13600000000;
            return applicableRate;
        }
        if ( block4 <= block.number && block5 > block.number ){
            applicableRate = 12500000000;
            return applicableRate;
        }
        if ( block5 <= block.number && block6 > block.number ){
            applicableRate = 11500000000;
            return applicableRate;
        }
    }

    
    function hasEnded() public constant returns (bool)
    {
        return block.number > block6;
    }
}