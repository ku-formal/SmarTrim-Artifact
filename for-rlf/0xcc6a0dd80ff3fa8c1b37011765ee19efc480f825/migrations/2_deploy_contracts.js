var contract = artifacts.require("EthereumDiamond"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
