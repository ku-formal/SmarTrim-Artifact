var contract = artifacts.require("SimpleEscrow"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
