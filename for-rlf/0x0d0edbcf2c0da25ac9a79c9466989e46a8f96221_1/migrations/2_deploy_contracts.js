var contract = artifacts.require("MyEthDice"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
