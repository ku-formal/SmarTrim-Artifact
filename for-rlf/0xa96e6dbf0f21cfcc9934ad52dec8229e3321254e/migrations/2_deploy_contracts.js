var contract = artifacts.require("FirstContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
