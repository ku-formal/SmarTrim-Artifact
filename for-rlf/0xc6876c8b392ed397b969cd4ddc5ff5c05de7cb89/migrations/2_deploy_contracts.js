var contract = artifacts.require("TokenERC20"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
