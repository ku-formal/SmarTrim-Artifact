var contract = artifacts.require("tokenContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
