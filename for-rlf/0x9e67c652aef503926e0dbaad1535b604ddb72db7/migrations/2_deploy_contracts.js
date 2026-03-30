var contract = artifacts.require("Bank"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
