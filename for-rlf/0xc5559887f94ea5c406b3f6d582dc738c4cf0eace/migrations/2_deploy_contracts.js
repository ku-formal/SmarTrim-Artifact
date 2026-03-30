var contract = artifacts.require("ETHLottery"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
