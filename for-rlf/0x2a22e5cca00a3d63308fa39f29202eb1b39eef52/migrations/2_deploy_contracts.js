var contract = artifacts.require("EDUToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
