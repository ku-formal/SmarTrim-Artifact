// require(myAddress.balance >= amount * sellPrice);
/* [FIX] Replace: "(myAddress.balance >= (amount * sellPrice))" => "(myAddress.balance <= (amount * sellPrice))" */

contract Test {
  function test (uint amount, uint sellPrice, uint balance) public {
    address myAddress = this;
    require (myAddress.balance > amount * sellPrice);
    //bool b = amount > sellPrice;
    //require(b);
    //uint c= amount+sellPrice;
  }
}
