var contract = artifacts.require("IGCoin"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
