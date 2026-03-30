/*
 * @source: etherscan.io 
 * @author: -
 * @vulnerable_at_lines: 29
 */

pragma solidity ^0.4.25;

contract W_WALLET
{
    struct Holder {
        uint unlockTime;
        uint balance;
    }
    mapping (address => Holder) public Acc1;
    mapping (address => Holder) public Acc2;
    
    function Put(uint _unlockTime) public payable
    {
        var acc = Acc1[msg.sender];
        acc.balance += msg.value;
        acc.unlockTime = _unlockTime>now?_unlockTime:now;
    }
    function Collect(uint _am) external payable {
        var acc = Acc2[msg.sender];
        if (acc.balance>=MinSum && acc.balance>=_am && now>acc.unlockTime) {
            if (msg.sender.call.value(_am)()) {
                acc.balance-=_am;
            }
        }
    }

    function() 
    public 
    payable
    {
        Put(0);
    }

    Log LogFile;

    uint public MinSum = 1 ether;    

    function W_WALLET(address log) public{
        LogFile = Log(log);
    }
}


contract Log 
{
    struct Message
    {
        address Sender;
        string  Data;
        uint Val;
        uint  Time;
    }

    Message[] public History;

    Message LastMsg;

    function AddMessage(address _adr,uint _val,string _data)
    public
    {
        LastMsg.Sender = _adr;
        LastMsg.Time = now;
        LastMsg.Val = _val;
        LastMsg.Data = _data;
        History.push(LastMsg);
    }
}