var contract = artifacts.require("UserContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
