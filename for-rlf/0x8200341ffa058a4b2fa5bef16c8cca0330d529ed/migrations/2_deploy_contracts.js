var contract = artifacts.require("CryptocarToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
