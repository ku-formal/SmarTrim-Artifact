var contract = artifacts.require("MultiSigWallet"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
