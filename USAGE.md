## Running SmarTrim on new other contracts

SmarTrim can be run as follows:

```bash
docker run --rm --init \
  -v "$(pwd)":/input \
  -v smartrim-artifact-solc-select:/root/.solc-select:ro \
  -w /root/VeriSmart --entrypoint /root/VeriSmart/main.exe \
  my-smartrim:fse26 exploit /input/<CONTRACT-FILE> [-t <TIMEOUT>] [-s <SOLVER-TIMEOUT>] \
  [-m <MAIN-CONTRACT>] [--solv <SOLIDITY-VERSION>] [-o <OUTDIR>] [-V]
```

* `-t`: tool timeout (in seconds, default: 1800)

* `-s`: SMT Solver timeout for each query (in milliseconds, default: 90000)

* `-m`: name of main contract. If not provided, SmarTrim assumes that the last contract in the source code is the main contract.

* `--solv`: Solidity version. Used to compile the contract.

* `-o`: output directory.

* `-V`: use the concrete validator to reproduce the attack in Foundry.

More information can be found using the `--help` command.

```bash
docker run --rm --init \
  -w /root/VeriSmart --entrypoint /root/VeriSmart/main.exe \
  my-smartrim:fse26 exploit --help
```

### A Running Example

* Assume we want to test a vulnerable contract [./SmarTrim/examples-public/leak_unsafe.sol](./SmarTrim/examples-public/leak_unsafe.sol). Since anyone can become the owner via calling the `setOwner` function, anyone can invoke the `exploit` function to drain all funds from the contract. 

* To generate a vulnerable transaction sequence for [./SmarTrim/examples-public/leak_unsafe.sol](./SmarTrim/examples-public/leak_unsafe.sol), run the following command:

```bash
docker run --rm --init \
  --volume "$(pwd)":/input \
  --volume smartrim-artifact-solc-select:/root/.solc-select:ro \
  --workdir /root/VeriSmart --entrypoint /root/VeriSmart/main.exe \
  my-smartrim:fse26 exploit /input/SmarTrim/examples-public/leak_unsafe.sol --kind el
```

* Then, you will see output on your terminal similar to the following.

```text
===== Report =====
[1] [EL] line 21, attacker.transfer(@Pay_amount@MEMO); : Disproven, 0.17s
[
  0:Example::Example
    []
    {msg.sender: 0x8006109F0688f89713A3eba9d54798C9DF3cA3E3; msg.value: 0}
    ;
  1:Example::setOwner
    [0x7fFFfFfFFFfFFFFfFffFfFfFfffFFfFfFffFFFFf]
    {msg.sender: 0xfFfFFffFffffFFffFffFFFFFFfFFFfFfFFfFfFfD; msg.value: 0}
    ;
  2:Example::exploit
    [0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF]
    {msg.sender: 0x7fFFfFfFFFfFFFFfFffFfFfFfffFFfFfFffFFFFf; msg.value: 0}
    
]
```