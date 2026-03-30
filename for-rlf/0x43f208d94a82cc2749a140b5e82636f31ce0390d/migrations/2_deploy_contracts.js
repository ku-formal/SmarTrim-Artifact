var contract = artifacts.require("betContractDOWN"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
