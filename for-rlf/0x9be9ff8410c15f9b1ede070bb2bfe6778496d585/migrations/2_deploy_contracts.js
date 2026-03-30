var contract = artifacts.require("NaviTokenBurner"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
