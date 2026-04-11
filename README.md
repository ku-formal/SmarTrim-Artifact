# SmarTrim-Artifact

`SmarTrim` is a symbolic executor for smart contracts driven by redundant transaction sequence pruning.

The purpose of this artifact is to enable replication of the paper's key experimental results by readers, i.e. Table 1 and Figure 3 in the paper.

The structure of our artifact is as follows:

* `./SmarTrim`: Source code of SmarTrim

* `./benchmark`: Our benchmark, including contracts, metadata, and ground truths

* `./scripts`, `./wrapper`: Scripts for experiments and data processing

* `./result`: A directory for storing summaries and raw results

## Requirements

Please refer to [REQUIREMENTS.md](./REQUIREMENTS.md).

## Installation

Please refer to [INSTALL.md](./INSTALL.md). 

You can also check a basic running example of SmarTrim (dockerized version) in [INSTALL.md](./INSTALL.md).

## Experiment

Execute

```bash
wrapper/run.sh
```

## Reproducing Table 1 (Section 6.1)
Please refer to [Table1.md](./Table1.md).


## Reproducing Figure 3 (Section 6.2)
Please refer to [Figure3.md](./Figure3.md).


## Configuration

* We ran the experiment simultaneously using 24 cores (exception: 3 subprocesses for `rlf`, due to its high memory usage). The number of cores to use is managed in the `config.json` file. For example, if you want to run `confuzzius` on RE Dataset with 40 cores, modify this file as follows:

```json
{
    "jobs": {
        "ls:rlf": 3,
        "re:confuzzius": 40,
        "default": 24
    }, ...
}
```

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


