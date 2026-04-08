# SmarTrim-Benchmark

### Structure of Contents

* `contracts`: Solidity smart contracts file.
  * `contracts/io`: Contains 487 Solidity contracts (273 after deduplication) reported in CVE due to integer over/underflow vulnerabilities.
  * `contracts/ls`: Contains 1,015 Solidity contracts (491 after deduplication) that have potential Ether leak and suicidal vulnerabilities.
  * `contracts/re`: Contains 69 Solidity contracts (49 after deduplication) with reentrancy vulnerabilities.
* `meta`: This directory contains some metadata for contracts such as main contract name, contract address, etc.
  * `io.csv`
  * `ls.csv`
  * `re.csv`
    * `id`: Solidity contract id
    * `main_name`: main contract name
    * `actual_order`: Experiment order. **A contract is included in a real experiment if and only if `actual_order` is not null.** 
* `labels`: Ground truths
  * `io.csv`
  * `ls.csv`
  * `re.csv`
    * `id`: Solidity contract id
    * `<bug-id>`: Line numbers with bugs of type `<bug-id>`. Separated by '/'.
    * `<bug-id>-f`: Functions that contain bugs of type `<bug-id>`. Note that multiple functions may exist that cause bug in the same location. In such cases, we use the separator ‘:’ instead of ‘/’.
    * `Explanation`: Reasons for TPs. Provided only for 407 contracts from `SPCon` and `PrettySmart`.
    * `Note`: Any other notes, including reasons for some (nontrivial) FPs.
  * `l4.csv`: Ground truths for bugs whose length is 4 or more (Section 6.2).
* `misc`
  * `selectors/<dataset>.csv`: DB for all function selectors in our dataset. Useful when grading.
* `sandbox`: Contains some proof-of-concept transaction sequences for showing vulnerabilities. Uses `foundry` framework.
  * build: `forge build --root sandbox`
  * test: `forge test --root sandbox` (If the project is not built, this command automatically builds)
    
## Sources

* (io, ls) VeriSmart dataset: https://github.com/kupl/VeriSmart-benchmarks
* (ls) PrettySmart dataset: https://github.com/Z-Zhijie/PrettySmart
  * How to get all true positive contracts in the original PrettySmart experiment: Go [experiment/detection_result](https://github.com/Z-Zhijie/PrettySmart/tree/70edb8f310584d2e558a489a7c841df17195817a/experiment/detection_result) and download `Smartbugs_comparedTools` and `Smartbugs_labeledRes`. See `T/F` column or `label` column.
* (ls) SPCon dataset: https://github.com/Franklinliu/SpCon-Artifact
  * [CVEAccessControlResults](https://github.com/Franklinliu/SpCon-Artifact/tree/master/ISSTA2022Result/CVEAccessControlResults)
  * [SmartBugsWildDataset](https://github.com/Franklinliu/SpCon-Artifact/tree/master/ISSTA2022Result/SmartBugsWildResults)
    * [TP informations](ISSTA2022Result/SmartBugsWildResults/samplesForAccuracyEvaluation)
* (re) SmartFix dataset: https://github.com/kupl/SmartFix-Artifact
* (re) Smartian dataset: https://github.com/SoftSec-KAIST/Smartian-Artifact
* (re) Sailfish dataset: https://github.com/ucsb-seclab/sailfish

## Abbreviations

Some explanations for EL/SU bugs in `labels/ls.csv` contain the following abbreviations:

* EL: Ether leak
* SU: Suicidal
* BR: Bad randomness
* FR: Front-running attack
* TL: Token leak 
* PB: Permission bug
* PIB: Possibly intended behavior
* KHP: Known honeypot patterns
