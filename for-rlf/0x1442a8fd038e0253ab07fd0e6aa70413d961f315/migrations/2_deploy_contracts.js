var contract = artifacts.require("BurnableOpenPaymentFactory"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
