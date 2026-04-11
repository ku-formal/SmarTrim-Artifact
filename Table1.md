## Reproducing Table 1 (Section 6.1)

### Full Experiment
* **Running Experiments**. Execute
```
wrapper/run-exp1.sh
```
The expected total runtime of the script is **7d 18h 30m**:
  
|Tool|LS Dataset|IO Dataset|RE Dataset|
|---|---|---|---|
|**SmarTrim**|5h|6h|30m|
|**AChecker**|1h|_n/a_|_n/a_|
|**Confuzzius**|6h 30m|5h|1h|
|**EF/CF**|11h|_n/a_|1h|
|**LENT-SSE**|22h|17h|4h|
|**Mythril**|9h|6h|1h|
|**RLF**|2d 14h|_n/a_|_n/a_|
|**Sailfish**|_n/a_|_n/a_|<2m|
|**SliSE**|_n/a_|_n/a_|30m|
|**Slither**|<1m|_n/a_|<1m|
|**SmarTest**|6h|6h|_n/a_|
|**Smartian**|10h|6h|1h|
|**_Total_**|**_XXX_**|**_XXX_**|**_XXX_**|

  

* **Generating Table 1**. After the script ``wrapper/run-exp1.sh`` terminates, execute
```
wrapper/interp-exp1.sh
```
This will generate `~/SmarTrim-Artifact/result/summary/<dataset>/<tool>.csv` and `~/SmarTrim-Artifact/result/summary/table.csv`. The latter corresponds to Table 1 in Section 6.1.

* **Interpreting Table 1**. The meaning of each column in Table 1 (`~/SmarTrim-Artifact/result/summary/table.csv`) is as follows.
  * `el-f`, `su-f`, `io-f`, `re-f`: The number of functions detected by each tool, which have EL, SU, IO, and RE vulnerability, respectively. Correspond to `All`-`Func` columns in Table 1.

  * `el`, `su`, `io`, `re`: The number of lines detected by each tool, which have EL, SU, IO, and RE vulnerability, respectively. Correspond to `All`-`Line` columns in Table 1.

  * `el-f-com`, `su-f-com`, `io-f-com`, `re-f-com`: The number of vulnerable functions, but the results are on contracts that were commonly examined by all tools without timeouts or runtime errors. Correspond to `Com`-`Func` columns in Table 1.

  * `el-com`, `su-com`, `io-com`, `re-com`: The number of vulnerable lines, but the results are on contracts that were commonly examined by all tools without timeouts or runtime errors. Correspond to `Com`-`Line` columns in Table 1.

  * `total-f`, `total`: The number of total vulnerable functions (`el-f` + `su-f` + `io-f` + `re-f`) and total vulnerable lines (`el` + `su` + `io` + `re`), respectively. Correspond to `Total` column in Table 1.

    * `el-f-fp`, `el-fp`, `su-f-fp`, `su-fp`, `re-f-fp`, `re-fp`, `total-f-fp`, `total-fp`: Same as above, but these are the number of false positives. 
  
      * Note that this process can be automated because, for EL/SU/RE bugs, we are treating bugs that do not exist in the ground truth as false positives (Section 6.1, 'ground truth' paragraph). This was possible because we iterated through the detection results from each tool and added new bugs to the ground truth if found. For IO bugs, we do not count the number of false positives.

  * `el-precision`, `su-precision`, `re-precision`, `total-precision`: #TP / (#TP + #FP) of each tool. See Section 7, 'False Positive' paragraph.
  
### Small-scale Experiment (Partial reproduction of Table 1)

Since fully reproducing Table 1 by running ``wrapper/run-exp1.sh`` takes a long time (7d 18h 30m), we provide instructions for a small-scale experiment that reproduces only the SmarTrim results in Table 1. Please refer to [Table1-Partial.md](Table1-Partial.md).
