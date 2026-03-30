import os
import sys
import time
import csv
import subprocess
from datetime import datetime
import argparse
from multiprocessing import Pool, Lock, Value

LOCK = Lock ()
cnt = Value ('i', 0)

field_summary = ["name", "time", "iter", "path", "total", "proven", "unproven",
                 "IO", "IO_proven", "IO_unproven", "DZ", "DZ_proven", "DZ_unproven",
                 "nontirivial"]
field_time = ["name", "time"]

solc_lst = ["0.4.16", "0.4.17", "0.4.18", "0.4.19", "0.4.20", "0.4.21", "0.4.23", "0.4.24", "0.4.25", "0.4.26",
            "0.5.0",  "0.5.1",  "0.5.2",  "0.5.3",  "0.5.4",  "0.5.5",  "0.5.6",  "0.5.7",  "0.5.8", "0.5.9", "0.5.10",
            "0.5.11", "0.5.12", "0.5.13", "0.5.14", "0.5.15", "0.5.16", "0.5.17",
            "0.6.0", "0.6.1", "0.6.2", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.7", "0.6.8", "0.6.9", "0.6.10",
            "0.6.11", "0.6.12",
            "0.7.0", "0.7.1", "0.7.2", "0.7.3", "0.7.4", "0.7.5", "0.7.6",
            "0.8.0", "0.8.1", "0.8.2", "0.8.3", "0.8.4", "0.8.5", "0.8.6"]

def get_solc_version (row):
  idx = row['compiler_version'].find ('+')
  # assert (not (idx == -1)), "compiler version : " + row['compiler_version'] # there exist few exceptional cases
  solv = (row['compiler_version'])[1:idx]

  for solv in list(reversed(solc_lst)): # To avoid mismatch, iterate in reversed order (e.g., mismatch 0.6.11 to 0.6.1 )
    if solv in row['compiler_version']:
      return solv
  return "0.4.25" #NotImplemented

def run (args):
  process, length, row, pgmdir, outdir = args[0], args[1], args[2], args[3], args[4]
  z3timeout, global_timeout, inline_depth, kill_timeout, inline_enforce = args[5], args[6], args[7], args[8], args[9]

  summary = os.path.join (outdir,"summary.csv")
  timeinfo = os.path.join (outdir, "time.csv") # Do not name it as 'time'; conflict with 'time' module
  history = os.path.join (outdir, "cmd_history.txt")

  fbase = row["id"]
  fname = fbase + ".sol"
  main_name = row["main_name"]

  solc = get_solc_version (row)
  cmd = ["timeout", str(kill_timeout), "./main.native", "-exp", "-input", os.path.join(pgmdir, fname), "-main", main_name, "-solc", solc,
         "-z3timeout", str(z3timeout), "-verify_timeout", str(global_timeout), "-inline_depth", str(inline_depth),
         "-verbose"]
  cmd.append("-inline_enforce") if inline_enforce else cmd

  BEFORE = time.time ()
  p = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
  cmdout = p.stdout.read()
  AFTER = time.time ()
  p.wait ()
  p.poll ()

  try:
    f_media = open ("media" + "_" + fbase, 'r')
    f_summary = open (summary, 'a')
    f_summary.write (f_media.readline() + "\n")

    f_media.close()
    f_summary.close()
    os.remove ("media" + "_" + fbase)
  except IOError: # media and training file may not exist, e.g., compile error, out of memory
    f_summary = open (summary,'a')
    if "Fatal error: out of memory." in cmdout.decode("utf-8"):
      f_summary.write (fname + "," + "OUT OF MEMORY" + "\n")
    elif p.returncode == 124:
      f_summary.write (fname + "," + "TIMEOUT" + "\n")
    else:
      lines = cmdout.decode("utf-8").split("\n")
      lines = [l for l in lines if "Error" in l or "error" in l or "Failure" in l]
      err_msg = lines[0].replace('\n','') if len(lines) > 0 else "ERROR"
      f_summary.write (fname + "," + err_msg + "\n")
    f_summary.close()

  f_res = open (os.path.join(outdir, fname.replace (".sol", ".txt")) , 'w')
  f_time = open (timeinfo, 'a')
  f_history = open (history, 'a')

  f_res.write (cmdout.decode("utf-8"))
  f_time.write (fbase + "," + str(AFTER-BEFORE) + "\n")
  f_history.write (" ".join (cmd) + "," + str ((datetime.now())) + "\n")

  f_res.close ()
  f_time.close ()
  f_history.close ()


def run_sequential (args_list):
  for args in args_list:
    run (args)


def get_il (rows, pgmdir, outdir):
  for row in rows:
    cmd = ["./main.native", "-input", os.path.join(pgmdir, row['FileName']), "-main", row['MainContractName'], "-il"]
    p = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
    cmdout = p.stdout.read()
    f = open (os.path.join(outdir, row['FileName'].replace (".sol", ".txt")) , 'w')
    idx = cmdout.decode("utf-8").find ("\"}\ncontract")
    assert (not (idx == -1))
    f.write (cmdout.decode("utf-8")[idx+3:])
    f.close ()


def main ():
  parser = argparse.ArgumentParser()

  parser.add_argument ('--root_info', type=str)
  parser.add_argument ('--pgmdir', type=str)
  parser.add_argument ('--outdir', type=str)
  parser.add_argument ('--include', type=str, default='')
  parser.add_argument ('--exclude', type=str, default='')
  parser.add_argument ('--z3timeout', type=int, default=10000)
  parser.add_argument ('--global_timeout', type=int, default=60)
  parser.add_argument ('--inline_depth', type=int, default=2)
  parser.add_argument ('--process', type=int, default=0)
  parser.add_argument ('--pgmnum', type=int, default=0)
  parser.add_argument ('--kill_timeout', type=int, default=3600)
  parser.add_argument ('--dedup', default=True, action='store_true')
  parser.add_argument ('--no-dedup', dest='dedup', action='store_false')
  parser.add_argument ('--inline_enforce', default=False, action='store_true')

  args = parser.parse_args ()

  csvfile = open (args.root_info, 'r')
  reader = csv.DictReader (csvfile)
  rows = list (reader)
  csvfile.close()

  include = []
  if args.include == "":
    include = [row['id'] for row in rows]
  else:
    f_include = open (args.include, 'r')
    include = [line.replace("\r\n","").replace ("\n","") for line in f_include]
    f_include.close()

  exclude = []
  if args.exclude =="":
    exclude = []
  else:
    f_exclude = open (args.exclude, 'r')
    exclude = [line.replace("\r\n","").replace ("\n","") for line in f_exclude]
    f_exclude.close()

  with open (os.path.join (args.outdir, "summary.csv"), 'w') as f:
    writer = csv.DictWriter(f, fieldnames=field_summary)
    writer.writeheader()

  with open (os.path.join (args.outdir, "time.csv"), 'w') as f:
    writer = csv.DictWriter(f, fieldnames=field_time)
    writer.writeheader()

  rows = list (filter (lambda row: not (row['actual_order'] == ""), rows)) if args.dedup else rows
  rows = list (filter (lambda row: not ('vyper:' in row['compiler_version']), rows))
  rows = list (filter (lambda row: row['id'] in include, rows))
  rows = list (filter (lambda row: not (row['id'] in exclude), rows))
  rows = rows[:args.pgmnum] if args.pgmnum > 0 else rows

  length = len (rows)
  args_list = list (map (lambda row: [args.process, length, row, args.pgmdir, args.outdir,
                                      args.z3timeout, args.global_timeout, args.inline_depth, args.kill_timeout, args.inline_enforce], rows))
  print ("# of pgms to be analyzed: " + str(length))
  BEFORE = time.time()
  if args.process == 0:
    run_sequential (args_list)
  else:
    pool = Pool (args.process)
    pool.map (run, args_list)
    pool.close ()
    pool.join ()
  # getIL (rows, args.pgmdir, args.outdir)
  AFTER = time.time()

  print ("Took: " + str(AFTER-BEFORE))
  took = open (os.path.join (args.outdir, 'took.txt'), 'w')
  took.write (str (AFTER-BEFORE))
  took.close()

  with open(os.path.join (args.outdir, "summary.csv"), 'r') as f_input:
    csv_input = csv.DictReader(f_input)
    data = sorted(csv_input, key=lambda row: row['name'])

  with open(os.path.join (args.outdir, "summary.csv"), 'w') as f_output:
    csv_output = csv.DictWriter(f_output, fieldnames=field_summary)
    csv_output.writeheader()
    csv_output.writerows(data)

  with open(os.path.join (args.outdir, "time.csv"), 'r') as f_input:
    csv_input = csv.DictReader(f_input)
    data = sorted(csv_input, key=lambda row: row['name'])

  with open(os.path.join (args.outdir, "time.csv"), 'w') as f_output:
    csv_output = csv.DictWriter(f_output, fieldnames=field_time)
    csv_output.writeheader()
    csv_output.writerows(data)


if __name__ == "__main__":
  main()
