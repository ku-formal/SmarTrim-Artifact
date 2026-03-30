var contract = artifacts.require("LuckyNumberService"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
