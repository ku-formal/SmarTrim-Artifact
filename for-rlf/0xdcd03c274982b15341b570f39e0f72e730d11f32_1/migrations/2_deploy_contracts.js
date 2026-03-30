var contract = artifacts.require("BitherumToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
