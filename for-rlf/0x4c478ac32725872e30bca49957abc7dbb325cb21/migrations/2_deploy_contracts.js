var contract = artifacts.require("COIN"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
