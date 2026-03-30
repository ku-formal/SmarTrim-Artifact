var contract = artifacts.require("BtcToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
