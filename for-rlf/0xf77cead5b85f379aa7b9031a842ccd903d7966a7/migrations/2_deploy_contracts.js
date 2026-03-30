var contract = artifacts.require("DummyBancorToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
