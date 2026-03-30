var contract = artifacts.require("ProxyAdmin"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
