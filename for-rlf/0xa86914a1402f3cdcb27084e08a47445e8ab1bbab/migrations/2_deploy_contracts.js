var contract = artifacts.require("ECR20HoneycombToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
