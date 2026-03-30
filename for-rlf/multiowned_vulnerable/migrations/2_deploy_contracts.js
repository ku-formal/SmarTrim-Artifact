var contract = artifacts.require("TestContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
