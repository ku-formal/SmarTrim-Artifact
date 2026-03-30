var contract = artifacts.require("BuyerToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
