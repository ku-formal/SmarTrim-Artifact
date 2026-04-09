# SmarTrim-Artifact

`SmarTrim` is a symbolic executor for smart contracts driven by redundant transaction sequence pruning.

We describe the replication methods for Table 1 and Figure 3 in the table. The structure of our artifact is as follows:

* `./SmarTrim`: Source code of SmarTrim

* `./benchmark`: Our benchmark, including contracts, metadata, and ground truths

* `./scripts`, `./wrapper`: Scripts for experiments and data processing

* `./result`: A directory for storing summaries and raw results

## Requirements

Please refer [REQUIREMENTS.md]().

## Installation

Please refer [INSTALL.md](). 

You can also check a basic running example of SmarTrim (dockerized version) in [INSTALL.md]().

## Experiment

Execute

```bash
wrapper/run.sh
```

## Reproducing Table 1 (Section 6.1)

**Experiment**. Execute

```bash
wrapper/run-exp1.sh
```

**Data Processing**. Execute

```bash
wrapper/interp-exp1.sh
```

This will generate `~/SmarTrim-Artifact/result/summary/<dataset>/<tool>.csv` and `~/SmarTrim-Artifact/result/summary/table.csv`. **`~/SmarTrim-Artifact/result/summary/table.csv` corresponds to Table 1 in Section 6.1.**

* `el-f`, `su-f`, `io-f`, `re-f`: The number of functions detected by each tool, which have EL, SU, IO, and RE vulnerability, respectively. Correspond to `All`-`Func` columns in Table 1.

* `el`, `su`, `io`, `re`: The number of lines detected by each tool, which have EL, SU, IO, and RE vulnerability, respectively. Correspond to `All`-`Line` columns in Table 1.

* `el-f-com`, `su-f-com`, `io-f-com`, `re-f-com`: The number of vulnerable functions, but the results are on contracts that were commonly examined by all tools without timeouts or runtime errors. Correspond to `Com`-`Func` columns in Table 1.

* `el-com`, `su-com`, `io-com`, `re-com`: The number of vulnerable lines, but the results are on contracts that were commonly examined by all tools without timeouts or runtime errors. Correspond to `Com`-`Line` columns in Table 1.

* `total-f`, `total`: The number of total vulnerable functions (`el-f` + `su-f` + `io-f` + `re-f`) and total vulnerable lines (`el` + `su` + `io` + `re`), respectively. Correspond to `Total` column in Table 1.

  * `el-f-fp`, `el-fp`, `su-f-fp`, `su-fp`, `re-f-fp`, `re-fp`, `total-f-fp`, `total-fp`: Same as above, but these are the number of false positives.

  * `el-precision`, `su-precision`, `re-precision`, `total-precision`: #TP / (#TP + #FP) of each tool. See Section 7, 'False Positive' paragraph.

## Reproducing Figure 3 (Section 6.2)

**Experiment**. Execute

```bash
wrapper/run-exp2.sh
```

**Data Processing**. Execute

```bash
wrapper/interp-exp2.sh
```

This will generate `~/SmarTrim-Artifact/result/Inc-d4.pdf` and `~/SmarTrim-Artifact/result/Random-d4.pdf`, which corrrespond to Fig.3 (a) and Fig.3 (b) in Section 6.2, respectively.

## Configuration

* We ran the experiment simultaneously using 24 cores (exception: 3 subprocesses for `rlf`, due to its high memory usage). The number of cores to use is managed in the `config.json` file. For example, if you want to run `confuzzius` on RE dataset with 40 cores, modify this file as follows:

```json
{
    "jobs": {
        "ls:rlf": 3,
        "re:confuzzius": 40,
        "default": 24
    }, ...
}
```

## Expected Runtime

All tools were executed on 24 cores, except RLF (3 cores).

|Tool|LS|IO|RE|
|---|---|---|---|
|**SmarTrim-Inc+Pruning**|5h|6h|30m|
|**SmarTrim-Inc**|5h 30m|6h|30m|
|**SmarTrim-Random+Pruning**|4h|5h|30m|
|**SmarTrim-Random**|4h 30m|5h 30m|30m|
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
|**_Total_**|**_6d 2h 30m_**|**_2d 14h 30m_**|**_9h 30m_**|

## Tool Maintenance

SmarTrim will be maintained in a separate repository: [https://github.com/ku-formal/VeriSmart-public]()

## Contact

Hyegeun Song: [hyegeun_song@korea.ac.kr]()

## Appendix

### Testing a Single Tool

Run each program with

```bash
$ python scripts/execute.py -t <tool> -d <dataset>
# or
$ python scripts/execute.py --tool <tool> --dataset <dataset>
```

where

```
<tool> =
  | smartrim | smartrimbase
  | smartrimr | smartrimbaser
  | achecker | confuzzius | efcf 
  | lent | mythril | rlf
  | sailfish | slise | slither 
  | smartest | smartian
<dataset> = io | ls | re
```

`smartrimbase`, `smartrimr`, and `smartrimbaser` correspond to Inc, Random+Pruning, Random in Section 6.2, respectively.

The command will use `~/SmarTrim-Artifact/output/` as a workspace directory, and will finally store raw data in `~/SmarTrim-Artifact/result/<dataset>/<tool>`. Resulting format is as follows:

* `OUTPUT/cmd_history.csv`: command lines, start times, end times, and return codes
* `OUTPUT/took.txt`
* `OUTPUT/<sol-id>/.stdout.txt`: `stdout` of each tool
* `OUTPUT/<sol-id>/.stderr.txt`: `stderr` of each tool
* `OUTPUT/<sol-id>/(any other outputs of each tool)`

where `OUTPUT = ~/SmarTrim-Artifact/result/<dataset>/<tool>` and `<sol-id>` is ID of each contract in our benchmark.


