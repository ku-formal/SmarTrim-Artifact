# SmarTrim-Experiment (Submission for FSE 2026)

`SmarTrim` is a symbolic executor for smart contracts driven by redundant transaction sequence pruning.

We describe the replication methods for Table 1 and Figure 3 in the table. The structure of our artifact is as follows:

* `smartrim`: Source code of SmarTrim

* `smartrim-benchmarks`: Our [benchmark](https://anonymous.4open.science/r/SmarTrim-bench-FSE26-B2FF/README.md), including contracts, metadata and ground truths

* `smartrim-experiments`: scripts for experiments and data processing

* `smartrim-result`: [summaries and raw results](https://anonymous.4open.science/r/SmarTrim-Result-FSE26-4045/README.md)

If the link above does not work, please copy the link below directly:

* `smartrim`: https://anonymous.4open.science/r/SmarTrim-FSE26-F65E/README.md
* `smartrim-benchmarks`: https://anonymous.4open.science/r/SmarTrim-bench-FSE26-B2FF/README.md
* `smartrim-result`: https://anonymous.4open.science/r/SmarTrim-Result-FSE26-4045/README.md
* `smartrim-result` backup: https://anonymous.4open.science/r/SmarTrim-Result-2-8B82/README.md

### Requirements

* We recommend Ubuntu 22.04 or 24.04. (We tested on 22.04)

* We recommend computers with 64 threads or more and 62 GB RAM or more.

* `docker` ([install](https://docs.docker.com/engine/install/ubuntu/))

  * To run `docker` without `sudo`, execute `sudo usermod -aG docker $USER`. (ref: https://askubuntu.com/a/739861)

* `python` 3.9+ with `numpy`, `pandas`, `matplotlib`

### Installation

* We assume this directory is installed in your home, i.e. the paths should be `~/SmarTrim-Artifact`.

* Run

```bash
$ chmod +x wrapper/*.sh
$ wrapper/build.sh
```

### Reproducing Table 1 (Section 6.1)

**Experiment**

```bash
# If you want to reproduce Section 6.1 only, run:
wrapper/run-exp1.sh

# If you want to reproduce both Section 6.1 and 6.2, run:
wrapper/run.sh
```

**Data Processing**

```bash
python scripts/analyze_tool.py
```

This will generate `~/smartrim-result/summary/<dataset>/<tool>.csv` and `~/smartrim-result/summary/table.csv`. Read `~/smartrim-result/summary/table.csv` and compare to our Table 1.

### Reproducing Figure 3 (Section 6.2)

**Experiment**

```bash
wrapper/run-exp2.sh
```

**Data Processing**

```bash
python scripts/analyze_ablation.py
```

This will generate `~/smartrim-result/Inc-d4.pdf` and `~/smartrim-result/Random-d4.pdf`. Open this file and compare to our Figure 3.

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

### Testing a Single Tool

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


