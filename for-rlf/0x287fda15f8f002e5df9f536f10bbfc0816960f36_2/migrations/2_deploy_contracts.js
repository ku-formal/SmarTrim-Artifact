var contract = artifacts.require("EncryptedToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
