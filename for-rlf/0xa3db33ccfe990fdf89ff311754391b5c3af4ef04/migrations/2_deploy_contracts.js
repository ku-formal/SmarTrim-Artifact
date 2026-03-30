var contract = artifacts.require("CvcProxy"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
