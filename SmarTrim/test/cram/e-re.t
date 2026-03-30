reentrancy el experiments:
  $ cd ../..
unsafe
  $ verismart exploit test/sol/re-el-basic.sol --main RE --solv 0.8.18 --kind re --depth 3 --quiet \
  > --expect 1
safe
  $ verismart exploit test/sol/re-el-basic.sol --main RESafe --solv 0.8.18 --kind re --depth 3 --quiet \
  > --expect 0
unsafe interface
  $ verismart exploit test/sol/re-el-basic.sol --main REIntf --solv 0.8.18 --kind re --depth 3 --quiet \
  > --expect 1
  $ verismart exploit test/sol/re-el-basic.sol --main REIntf2 --solv 0.8.18 --kind re --depth 3 --quiet \
  > --expect 1
safe interface
  $ verismart exploit test/sol/re-el-basic.sol --main REIntfSafe --solv 0.8.18 --kind re --depth 3 --quiet \
  > --expect 0
