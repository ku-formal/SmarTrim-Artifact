var contract = artifacts.require("DepositContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
