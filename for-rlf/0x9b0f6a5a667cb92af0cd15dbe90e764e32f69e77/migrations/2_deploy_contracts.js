var contract = artifacts.require("DSTContract"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
