var contract = artifacts.require("HelloWorld"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
