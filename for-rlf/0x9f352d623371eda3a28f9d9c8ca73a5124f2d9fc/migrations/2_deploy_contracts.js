var contract = artifacts.require("NoteToken"); 
module.exports = function(deployer) {
   deployer.deploy(contract);
};
