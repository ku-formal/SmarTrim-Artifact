## Installation

* We assume this directory is installed in your home, i.e. the paths should be `~/SmarTrim-Artifact`.

* Run

```bash
$ chmod +x wrapper/*.sh
$ wrapper/build.sh
```

* *Estimated build time: 21 minutes*

* This command will install `uv` (A Python package manager) and 13 docker images in your machine.

### Basic Testing (A Running Example)

* Assume we want to test a vulnerable contract [./SmarTrim/examples-public/leak_unsafe.sol](). Since anyone can become the owner via calling `setOwner` function, anyone can invoke the `exploit` function to drain all funds from the contract. 

* To generate a vulnerable transaction sequence for [./SmarTrim/examples-public/leak_unsafe.sol](), run the following command:

```bash
docker run --rm --init \
  --volume "$(pwd)":/input \
  --volume smartrim-artifact-solc-select:/root/.solc-select:ro \
  --workdir /root/VeriSmart --entrypoint /root/VeriSmart/main.exe \
  my-smartrim:fse26 exploit /input/SmarTrim/examples-public/leak_unsafe.sol --kind el
```

* Then, you will see an output on your terminal similar to the following.

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