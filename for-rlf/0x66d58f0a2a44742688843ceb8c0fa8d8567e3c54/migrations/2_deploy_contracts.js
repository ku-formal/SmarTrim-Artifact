var contract = artifacts.require("DoubleOrNothingImpl"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
