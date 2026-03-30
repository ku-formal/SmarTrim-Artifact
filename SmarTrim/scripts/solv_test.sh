#!/bin/bash

OUTDIR=~/VeriSmart/solv_test_`date +%m%d_%H%M`

mkdir $OUTDIR

python3 ~/VeriSmart/scripts/script2.py \
  --root_info ~/benchmarks/meta/verified-210713-meta.csv \
  --pgmdir ~/benchmarks/verified-210713 \
  --outdir $OUTDIR \
  --z3timeout 10 \
  --global_timeout 30 \
  --kill_timeout 300 \
  --process 40
