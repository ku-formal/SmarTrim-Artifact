var contract = artifacts.require("KairosToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
