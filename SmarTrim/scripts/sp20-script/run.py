import commands
import os
import sys
import tempfile
import time
import csv
import subprocess
from datetime import datetime

rootdir = sys.argv[1]
pgmdir = sys.argv[2]
outdir = sys.argv[3]

tooltimeout = 60
z3timeout = 10000

#cmd = "./main.native -timeout 30 -z3timeout 100000 -exp2 -input "

summarydir = os.path.join(outdir,"summary.csv")
summaryfile = open (summarydir, 'w')
summaryfile.write ("name" + "," + "time" + "," + "iter" + "," + "path" + "," + "total" + "," + "proven" + "," + "unproven" + "," + "IO" + "," + "IO_proven" + "," + "IO_unproven" + "," + "DZ" + "," + "DZ_proven" + "," + "DZ_unproven" + "," + "nontrivial" + "\n")
# r.write ("name" + "," + "success/fail" + "\n")
summaryfile.close()

time_csv = os.path.join(outdir, "time.csv")
root_csv = csv.reader(open(rootdir))
root_csv = list (root_csv)

cmd_history = os.path.join(outdir, "cmd_history.txt")

files = os.listdir(os.path.abspath(pgmdir)) 
files.sort()
i = 0

for fname in files:
    i_fname = root_csv[i+1][0]
    i_rname = root_csv[i+1][1]
    assert i_fname == fname, 'file names do not coincide'
    print ("[" + str(i+1) + "]" + " " + "processing " + fname + " @ " + str ((datetime.now())))
    resultdir = os.path.join(outdir, fname.replace(".sol",".txt"))

    #os.system (cmd + pgmdir + f + " " + " > " + "./test/" + str(i))
    # os.system (cmd + pgmdir + f)

    before = time.time()
    cmd = "./main.native -solc 0.4.25 -verify_timeout " + str(tooltimeout) + " " + "-z3timeout " + str(z3timeout) + " " + "-exp -input " + os.path.join(pgmdir,i_fname) + " " + "-main " + i_rname
    (failed, res) = commands.getstatusoutput(cmd)
    # os.system (cmd " > " + "./result/" + str(i))
    after = time.time()

    media = open ("./media" + "_" + fname.replace(".sol", ""), 'r')
    summaryfile = open (summarydir,'a')
    summaryfile.write(media.readline() + "\n")
    media.close()
    summaryfile.close()
    os.remove ("media" + "_" + fname.replace(".sol", ""))

    resultfile = open(resultdir, 'a')
    resultfile.write(res)
    timefile = open(time_csv, 'a')
    timefile.write(fname.replace(".sol", "") + "," + str(after - before) + "\n")
    cmdfile = open(cmd_history, 'a')
    cmdfile.write (cmd + "," +  str ((datetime.now())) + "\n")

    resultfile.close()
    timefile.close()
    cmdfile.close()

    i = i+1
