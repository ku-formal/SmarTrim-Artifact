var contract = artifacts.require("BurnablePaymentFactory"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
