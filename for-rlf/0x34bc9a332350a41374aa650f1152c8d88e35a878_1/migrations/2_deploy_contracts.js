var contract = artifacts.require("GuessTheNumberGame"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
