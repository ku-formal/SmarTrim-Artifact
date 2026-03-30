var contract = artifacts.require("Escrow_v1_0"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
