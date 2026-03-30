var contract = artifacts.require("TestNetworkToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
