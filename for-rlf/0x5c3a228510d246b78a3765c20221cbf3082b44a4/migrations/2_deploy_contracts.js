var contract = artifacts.require("STQToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
