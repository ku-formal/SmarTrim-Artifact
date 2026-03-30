e-semi-extcall.t : 
  $ cd ../..
extcall succeed
  $ verismart exploit test/sol/el-semi-extcall.sol --main ELSemiExtcall --solv 0.8.18 --kind el \
  > --depth 3 --quiet --expect 1
extcall failed (due to zero address restriction)
  $ verismart exploit test/sol/el-semi-extcall.sol --main NoELSemiExtcall --solv 0.8.18 --kind el \
  > --depth 3 --quiet --expect 0
ether leak failed (extcall returns a safe address)
  $ verismart exploit test/sol/el-semi-extcall.sol --main AccessControlled --solv 0.8.18 --kind el \
  > --depth 3 --quiet --expect 0
validate succeed
  $ verismart exploit test/sol/el-semi-extcall.sol --main ELSemiExtcall --solv 0.8.18 --kind el \
  > --depth 3 --quiet --validate --expect 1
