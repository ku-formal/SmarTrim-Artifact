var contract = artifacts.require("TokensWarContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
