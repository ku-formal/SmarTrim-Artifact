## Reproducing Figure 3 (Section 6.2)

* **Prerequisite**. Before proceeding, you must have completed the chapter  **Reproducing Table 1 (Section 6.1)** (at least the **Small-scale experiment** in that chapter).
This is because ``Inc+Pruning`` in Figure 3 corresponds to SmarTrim in Table 1,
and we therefore reuse the SmarTrim data obtained from the previous experiment.
In other words, this chapter runs the remaining three variants in Figure 3 (``Inc``, ``Random``, and ``Random+Pruning``) and reproduces Figure 3 by combining their results with the previously obtained ``Inc+Pruning`` results.


* **Running Experiments**. Execute

```bash
wrapper/run-exp2.sh
```
The expected total runtime of the script is **1d 8h**:

|SmarTrim Variant |LS Dataset|IO Dataset|RE Dataset|Note|
|---|---|---|---|---|
|**Inc+Pruning**|n/a|n/a|n/a| Requires no additional time (see **Prerequisite**). |
|**Inc**|5h 30m|6h|30m| |
|**Random+Pruning**|4h|5h|30m| |
|**Random**|4h 30m|5h 30m|30m| |
|**_Total_**|**_XXX_**|**_XXX_**|**_XXX_**| |


* **Generating Figure 3**. Execute

```bash
wrapper/interp-exp2.sh
```
This will generate `~/SmarTrim-Artifact/result/Inc-d4.pdf` and `~/SmarTrim-Artifact/result/Random-d4.pdf`, which corrrespond to Fig.3 (a) and Fig.3 (b) in Section 6.2, respectively.
