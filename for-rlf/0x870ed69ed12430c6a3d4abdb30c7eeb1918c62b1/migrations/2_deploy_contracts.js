var contract = artifacts.require("RandomLedgerService"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
