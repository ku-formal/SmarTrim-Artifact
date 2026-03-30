var contract = artifacts.require("LuckyNumberImp"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
