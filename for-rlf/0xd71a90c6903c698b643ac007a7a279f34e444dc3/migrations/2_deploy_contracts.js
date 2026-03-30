var contract = artifacts.require("TimeLocker"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
