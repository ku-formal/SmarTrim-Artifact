/*
 * @source: http://blockchain.unica.it/projects/ethereum-survey/attacks.html#simpledao
 * @author: -
 * @vulnerable_at_lines: 19
 */

pragma solidity ^0.4.2;

contract SimpleDAO {
  mapping (address => uint) public credit;

  bool public enter = false;

  modifier nonReentrant {
    require (!enter);
    enter = true;
    _;
    enter = false;
  }

  function donate(address to) payable {
    require (credit[to] + msg.value >= credit[to]);
    credit[to] += msg.value;
  }

  function withdraw(uint amount) nonReentrant {
    if (credit[msg.sender]>= amount) {
      // <yes> <report> REENTRANCY
      bool res = msg.sender.call.value(amount)(); /* <RE_ENT>, <RE_VUL> */
      require(res);

      credit[msg.sender]-=amount;
    }
  }

  function queryCredit(address to) returns (uint){
    return credit[to];
  }
}
