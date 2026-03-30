contract Test {
  address owner = msg.sender;

  modifier onlyOwner {
    require(tx.origin == owner); // unsafe
  }

  modifier noContractsAllowed() {
    require(tx.origin == msg.sender, "No Contracts Allowed!"); // safe
    _;
  }

  modifier noHuman {
    require(msg.sender != tx.origin, "Do not send ETH directly"); // safe
  }

  function unsafe () public onlyOwner {
    uint a = 1;
  }

  function safe1 () public noContractsAllowed() {
    uint a = 1;
  }

  function safe2 () public noHuman() {
    uint a = 1;
  }
}
