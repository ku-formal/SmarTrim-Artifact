ether leak experiments:
  $ cd ../..
expect succeed
  $ verismart exploit test/sol/su-if-balance-zero.sol --solv 0.8.18 --kind el --depth 4 --quiet \
  > --expect 0
