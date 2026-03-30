var contract = artifacts.require("BuilderCrowdfunding"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
