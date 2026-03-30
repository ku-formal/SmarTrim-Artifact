var contract = artifacts.require("FrontToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
