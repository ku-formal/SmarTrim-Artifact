import os
import sys
import tempfile
import time
import csv
import subprocess
from datetime import datetime
import argparse 
from multiprocessing import Pool, Lock, Value
from os.path import expanduser

HOME = expanduser("~")
VERISMART = os.path.join (HOME, 'VeriSmart')

LOCK = Lock ()
cnt = Value ('i', 0)

solc_lst = ["0.4.16", "0.4.17", "0.4.18", "0.4.19", "0.4.20", "0.4.21", "0.4.23", "0.4.24", "0.4.25", "0.4.26",
            "0.5.0", "0.5.1", "0.5.2", "0.5.3", "0.5.4", "0.5.6", "0.5.7", "0.5.8", "0.5.9", "0.5.10",
            "0.5.11", "0.5.12", "0.5.13", "0.5.14", "0.5.15", "0.5.16", "0.5.17",
            "0.6.0", "0.6.1", "0.6.2", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.7", "0.6.8", "0.6.9", "0.6.10", "0.6.11", "0.6.12"]

def get_solc_version (row):
  for solv in list(reversed(solc_lst)): # To avoid mismatch, iterate in reversed order (e.g., mismatch 0.6.11 to 0.6.1 )
    if solv in row['compiler_version']:
      return solv
  return "0.4.25" #NotImplemented

def get_cmd (length, row, pgmdir, target, z3timeout, global_timeout, kill_timeout, bit, ngram, fname, main_name, infinite_ether, refined_vcgen, contract_init_eth, directed):
  solc = get_solc_version (row)
  l = ["timeout", str(kill_timeout),
       os.path.join(VERISMART, "main.native"),
       "-mode", "exploit", "-report", "-solv", solc,
       "-z3timeout", str(z3timeout), "-exploit_timeout", str(global_timeout)]

  for t in target:
    l.append (t)

  if not (bit==0):
    l.append ("-bit")
    l.append (str(bit))

  if not (ngram==0):
    l.append ("-ngram")
    l.append (str(ngram))

  l.append ("-input")
  l.append (os.path.join (pgmdir,fname))
  l.append ("-main")
  l.append (main_name)

  if infinite_ether:
    l.append ("-infinite_ether")

  if refined_vcgen:
    l.append ("-refined_vcgen")

  if contract_init_eth > 0:
    l.append ("-contract_init_eth")
    l.append (str(contract_init_eth))

  if directed:
    l.append ("-directed")

  LOCK.acquire ()
  cnt.value += 1
  print ("processing " + str(cnt.value) + "/" + str(length))
  print (" ".join (l))
  LOCK.release ()

  return l

def run (args):
  length, row, pgmdir, outdir, target = args[0], args[1], args[2], args[3], args[4]
  z3timeout, global_timeout, kill_timeout, bit, ngram =  args[5], args[6], args[7], args[8], args[9]
  infinite_ether, refined_vcgen, contract_init_eth, directed = args[10], args[11], args[12], args[13]

  timeinfo = os.path.join (outdir, "time.csv") # Do not name it as 'time'; conflict with 'time' module
  history = os.path.join (outdir, "cmd_history.txt")

  fbase = row['id']
  fname = fbase + ".sol"
  main_name = row['main_name']

  cmd = get_cmd (length, row, pgmdir, target, z3timeout, global_timeout, kill_timeout, bit, ngram, fname, main_name, infinite_ether, refined_vcgen, contract_init_eth, directed)

  BEFORE = time.time ()
  p = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
  cmdout = p.stdout.read()
  AFTER = time.time () # MUST be placed after the previous statement.
  p.wait() # somtimes processes are not immediately killed by 'timeout', so explicitly wait to terminate, in order to get proper return code.
  p.poll () # update p.returncode.

  LOCK.acquire ()

  # update time and executed command information
  f_res = open (os.path.join (outdir, fname.replace (".sol", ".txt")), 'w')
  f_time = open (timeinfo, 'a')
  f_history = open (history, 'a')

  f_res.write (cmdout.decode("utf-8"))
  f_time.write (fbase + "," + str (AFTER-BEFORE) + "\n")
  f_history.write (" ".join (cmd) + "," + str ((datetime.now())) + "\n")

  f_res.close()
  f_time.close()
  f_history.close()

  LOCK.release ()


def main():
  parser = argparse.ArgumentParser()

  parser.add_argument ('--root_info', type=str, help='directory of metafiles')
  parser.add_argument ('--pgmdir', type=str)
  parser.add_argument ('--include', type=str, default='', help='program lists to be included')
  parser.add_argument ('--exclude', type=str, default='', help='program lists to be excluded')
  parser.add_argument ('--outdir', type=str, help='directory for storing ouputs')
  parser.add_argument ('--target', type=str, nargs='+', default='all')
  parser.add_argument ('--z3timeout', type=int, default=60000)
  parser.add_argument ('--global_timeout', type=int, default=1800)
  parser.add_argument ('--kill_timeout', type=int, default=2100)
  parser.add_argument ('--bit', type=int, default=0)
  parser.add_argument ('--ngram', type=int, default=0)
  parser.add_argument ('--process', type=int, default=5)
  parser.add_argument ('--pgmnum', type=int, default=0)
  parser.add_argument ('--infinite_ether', default=False, action='store_true')
  parser.add_argument ('--refined_vcgen', default=False, action='store_true')
  parser.add_argument ('--contract_init_eth', type=int, default=0)
  parser.add_argument ('--directed', default=False, action='store_true')

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

  rows = list (filter (lambda row: not (row['actual_order'] == ""), rows))
  rows = list (filter (lambda row: row['id'] in include, rows))
  rows = list (filter (lambda row: not (row['id'] in exclude), rows))
  rows = rows[:args.pgmnum] if args.pgmnum > 0 else rows

  length = len (rows)

  args_list = list (
                map (lambda row:
                     [length,
                      row,
                      args.pgmdir,
                      args.outdir,
                      args.target,
                      args.z3timeout,
                      args.global_timeout,
                      args.kill_timeout,
                      args.bit,
                      args.ngram,
                      args.infinite_ether,
                      args.refined_vcgen,
                      args.contract_init_eth,
                      args.directed
                      ], rows))

  BEFORE = time.time()
  pool = Pool (args.process)
  pool.map (run, args_list)
  # https://stackoverflow.com/questions/35708371/purpose-of-pool-join-pool-close-in-multiprocessing
  pool.close () # pool.close tells the pool not to accept any new job.
  pool.join () # pool.join tells the pool to wait until all jobs finished then exit, effectively cleaning up the pool.
  AFTER = time.time ()

  print ("Took: " + str(AFTER-BEFORE))
  took = open (os.path.join (args.outdir, 'took.txt'), 'w')
  took.write (str (AFTER-BEFORE))
  took.close()

  # with open(os.path.join (args.outdir, "detailed_summary.csv"), 'r') as f_input:
  #  csv_input = csv.DictReader(f_input)
  #  data = sorted(csv_input, key=lambda row: row['file'])

  # with open(os.path.join (args.outdir, "detailed_summary.csv"), 'w') as f_output:
  #  csv_output = csv.DictWriter(f_output, fieldnames=fieldnames2)
  #  csv_output.writeheader()
  #  csv_output.writerows(data)

if __name__ == "__main__":
  main()
