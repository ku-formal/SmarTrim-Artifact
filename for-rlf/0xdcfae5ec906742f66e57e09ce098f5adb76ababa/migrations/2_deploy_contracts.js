var contract = artifacts.require("EthDeposit"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
