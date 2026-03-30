// SPDX-License-Identifier: MIT
pragma solidity ^0.4.25;

contract MigrationScript {
  address public owner;
  uint public last_completed_migration;

  event MigrationCompleted(uint migration_number);

  constructor() public {
    owner = msg.sender;
  }

  modifier restricted() {
    require(msg.sender == owner, "This function is restricted to the contract's owner");
    _;
  }

  function setCompleted(uint completed) public restricted {
    last_completed_migration = completed;
    emit MigrationCompleted(completed);
  }

  function upgrade(address new_address) public restricted {
    MigrationScript upgraded = MigrationScript(new_address);
    upgraded.setCompleted(last_completed_migration);
  }
}