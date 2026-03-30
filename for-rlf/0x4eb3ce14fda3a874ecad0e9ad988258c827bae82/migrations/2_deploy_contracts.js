var contract = artifacts.require("DepositBank"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
