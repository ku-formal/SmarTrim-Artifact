var contract = artifacts.require("BFTToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
