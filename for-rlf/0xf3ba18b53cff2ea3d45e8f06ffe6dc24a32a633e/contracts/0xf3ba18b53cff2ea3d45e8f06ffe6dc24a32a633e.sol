

contract Game_1
{
    string public question;
    bytes32 responseHash;
    mapping(bytes32 => bool) gameMaster;

    function Guess(string _response) external payable
    {
        require(msg.sender == tx.origin);
        if(responseHash == keccak256(_response) && msg.value >= 0.25 ether)
        {
            msg.sender.transfer(this.balance);
        }
    }

    function Start(string _question, string _response) public payable onlyGame_1Master {
        if(responseHash==0x0){
            responseHash = keccak256(_response);
            question = _question;
        }
    }

    function Stop() public payable onlyGame_1Master {
        msg.sender.transfer(this.balance);
    }

    function StartNew(string _question, bytes32 _responseHash) public payable onlyGame_1Master {
        question = _question;
        responseHash = _responseHash;
    }

    constructor(bytes32[] _gameMasters) public{
        for(uint256 i=0; i< _gameMasters.length; i++){
            gameMaster[_gameMasters[i]] = true;        
        }       
    }

    modifier onlyGame_1Master(){
        require(gameMaster[keccak256(msg.sender)]);
        _;
    }

    function() public payable{}
}
    
contract Game is Game_1 {
    constructor() Game_1(0x456) public { }
}
