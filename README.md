# SmarTrim-Experiment (Submission for FSE 2026)

`SmarTrim` is a symbolic executor for smart contracts driven by redundant transaction sequence pruning.

We describe the replication methods for Table 1 and Figure 3 in the table. The structure of our artifact is as follows:

* `./SmarTrim`: Source code of SmarTrim

* `./benchmark`: Our [benchmark](https://anonymous.4open.science/r/SmarTrim-bench-FSE26-B2FF/README.md), including contracts, metadata and ground truths

* `./scripts`: scripts for experiments and data processing

* `./result`: [summaries and raw results](https://anonymous.4open.science/r/SmarTrim-Result-FSE26-4045/README.md)

### Requirements

Please refer [REQUIREMENTS.md]().

### Installation

Please refer [INSTALL.md]().

### Experiment

Execute

```bash
wrapper/run.sh
```

### Reproducing Table 1 (Section 6.1)

```bash
python scripts/analyze_tool.py
```

This will generate `~/SmarTrim-Artifact/result/summary/<dataset>/<tool>.csv` and `~/SmarTrim-Artifact/result/summary/table.csv`. `~/smartrim-result/summary/table.csv` corresponds to Table 1 in Section 6.1.

### Reproducing Figure 3 (Section 6.2)

```bash
python scripts/analyze_ablation.py
```

This will generate `~/SmarTrim-Artifact/result/Inc-d4.pdf` and `~/SmarTrim-Artifact/result/Random-d4.pdf`, which corrrespond to Fig.3 (a) and Fig.3 (b) in Section 6.2, respectively.

### Configuration

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

### Expected Runtime

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

### Appendix

**Testing a Single Tool**

Run each program with

```bash
$ python scripts/x.py -t <tool> -d <dataset>
# or
$ python scripts/x.py --tool <tool> --dataset <dataset>
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

The command will use `~/smartrim-experiment/output/` as a workspace directory, and will finally store raw data in `~/smartrim-experiment/output-<dataset>-<tool>/`. Resulting format is as follows:

* `OUTPUT/cmd_history.csv`: command lines, start times, end times, and return codes
* `OUTPUT/took.txt`
* `OUTPUT/<sol-id>/.stdout.txt`: `stdout` of each tool
* `OUTPUT/<sol-id>/.stderr.txt`: `stderr` of each tool
* `OUTPUT/<sol-id>/(any other outputs of each tool)`

where `OUTPUT = ~/smartrim-experiment/output-<dataset>-<tool>/` and `<sol-id>` is ID of each contract in our benchmark.


