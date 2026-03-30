var contract = artifacts.require("CryptoCupToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
