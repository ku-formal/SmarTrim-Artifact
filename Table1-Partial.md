### Small-scale Experiment (Partial reproduction of Table 1)

Since fully reproducing Table 1 by running ``wrapper/run-exp1.sh`` takes a long time (7d 19h 30m),
we provide instructions for a small-scale experiment that reproduces only the SmarTrim results in Table 1.

* **Running Experiments**. Execute

```
wrapper/run-smartrim-only.sh
```
The expected total runtime of the script is **11h 30m** (corresponding to the SmarTrim row in the expected runtime table of [Table1.md](Table1.md)).

* **Generating Table 1 (SmarTrim row only)**. Execute
```
wrapper/interp-exp1.sh
```
As in the ``Reproducing Table 1 (Section 6.1)``, this script generates a table summarizing the results. However, the generated table contains only the SmarTrim row in Table 1.
