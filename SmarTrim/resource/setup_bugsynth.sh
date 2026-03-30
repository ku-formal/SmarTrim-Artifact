# !/bin/bash

# run the command in the directory where 'main.native' exists
python3 -c 'from src.bugsynth.py.mygensim import *; download_fastText()'
