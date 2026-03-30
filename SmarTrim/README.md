# Implementation of SmarTrim (Submission for FSE 2026)

This repository contains the actual implementation of `SmarTrim`. It is built on top of `SmarTest`, an [open-sourced](https://github.com/kupl/VeriSmart-public) Solidity smart contracts symbolic executor (which is noticed in Section 5 of our paper).

# Structures

The following parts of this project's structure are relevant to our paper:

* `src/exploit`
  * `run.ml`: implements Algorithm 1 (Section 3.1).
  * `db.ml`: manages V (line 5) in Algorithm 1 (Section 3.1).
  * `pruning.ml`: implements `subsumed` predicate (line 12) of Algorithm 1 and optimization techniques (Section 4.2).
  * `simplif.ml`: implements constraint simplification (Section 4.1).
* `src/validate`: implements a concrete validator (Section 5).

# A Running Example

To generate an attack transaction sequence for `examples-public/leak_unsafe.sol`, run:

```bash
./main.exe exploit examples-public/leak_unsafe.sol --timeout 1800 --solver-timeout 90000 --kind el
# or
./main.exe exploit examples-public/leak_unsafe.sol -t 1800 -s 90000 -k el
```

Then, you will get the following report:

```
===== Report =====
[1] [EL] line 21, attacker.transfer(@Pay_amount@MEMO); : Disproven, 0.08s
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
============ Statistics ============
# Iter                 : 14
- last disproven iter  : 14
- last disproven time  : 0.077
# Max explored depth   : 3
# Max disproven depth  : 2
# Formulas             : 7
# Timeout-formulas     : 0
# Queries              : 1
# Disproven            : 1
- ether leak           : 1
# Exit coverage        : nan% (0/0)

Time Elapsed (Real) : 0.207309007645
Time Elapsed (CPU)  : 0.078672
```

Run `./main.exe exploit --help` for more options.

# Native Build

1. Install `opam`, the OCaml package manager.
2. Run `opam install . --deps-only` to install all dependencies. (Note: This step will install `dune`, an OCaml project builder.)
3. Run `dune build`.
