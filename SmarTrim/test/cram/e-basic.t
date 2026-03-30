ether leak experiments:
  $ cd ../..
expect succeed
  $ verismart exploit test/sol/el-basic.sol --main EL --solv 0.6.12 --kind el --depth 4 --quiet \
  > --expect 1
expect failed
  $ verismart exploit test/sol/el-basic.sol --main EL --solv 0.6.12 --kind el --depth 4 --quiet \
  > --expect 2
  [88]
expect succeed, tests that EL in previous transactions does not affect the current one
  $ verismart exploit test/sol/el-basic.sol --main EL2 --solv 0.6.12 --kind el --depth 4 --quiet \
  > --expect 1
