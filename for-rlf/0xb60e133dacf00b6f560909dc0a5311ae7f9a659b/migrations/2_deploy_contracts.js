var contract = artifacts.require("GymToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
