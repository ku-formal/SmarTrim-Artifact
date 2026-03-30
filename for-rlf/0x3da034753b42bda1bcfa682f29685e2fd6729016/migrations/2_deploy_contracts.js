var contract = artifacts.require("VoipToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
