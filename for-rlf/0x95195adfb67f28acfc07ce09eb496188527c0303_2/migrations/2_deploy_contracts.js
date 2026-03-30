var contract = artifacts.require("CNEXToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
