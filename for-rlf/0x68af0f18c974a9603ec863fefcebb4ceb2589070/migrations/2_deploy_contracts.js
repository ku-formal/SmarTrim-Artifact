var contract = artifacts.require("PIGGYBANK"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
