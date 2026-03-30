import os
import sys
import time
import csv
import subprocess
from datetime import datetime
import argparse

def is_consistent (row, row_answer):
  assert (row['name'] == row_answer['name'])

  b1 = row['path'] == row_answer['path']
  b2 = row['unproven'] == row_answer['unproven']
  b3 = row['IO_unproven'] == row_answer['IO_unproven']
  b4 = row['DZ_unproven'] == row_answer['DZ_unproven']
  b5 = abs(float(row['time']) - float(row_answer['time'])) <= 12.0

  return b1 and b2 and b3 and b4 and b5


def check_diff (rows, rows_answer):
  assert (len(rows) == len(rows_answer))

  allTrue = all (is_consistent (row,row_answer) for row, row_answer in zip(rows,rows_answer))

  if allTrue:
    print ('Consistent')
  else:
    print ('Inconsistent')
    for row, row_answer in zip(rows,rows_answer):
      try:
        if not (is_consistent(row,row_answer)):
          print (row['name'] + ', ' +
                 '#path  : '     + str(int(row['path']) - int(row_answer['path'])) + ', ' +
                 '#alarm : '    + str(int(row['unproven']) - int(row_answer['unproven'])) + ', ' +
                 'time   : '    + str(float(row['time']) - float (row_answer['time'])))
      except ValueError:
          print (row['name'] + ', ' + 'Error')

def check_cve (outdir):
  csvfile = open(os.path.join(os.getenv("HOME"), "VeriSmart/scripts/sp20-script/cve-answer.csv"), 'r')
  reader = csv.DictReader(csvfile)
  rows_answer = list(reader)
  csvfile.close()

  cmd = ["python3", "scripts/script2.py", "--root_info", os.path.join(os.getenv("HOME"), "benchmarks/meta/cve-meta.csv"),
         "--pgmdir", os.path.join(os.getenv("HOME"), "benchmarks/cve"),
         "--outdir", outdir,
         "--z3timeout", "10000", "--global_timeout", "60",
         "--kill_timeout", "600",
         "--include", os.path.join(os.getenv("HOME"), "benchmarks/cve_sp20.txt"),
         "--no-dedup"]

  p = subprocess.Popen (cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
  p.wait()
  p.poll()

  csvfile = open (os.path.join (outdir, "summary.csv"), 'r')
  reader = csv.DictReader(csvfile)
  rows = list (reader)
  csvfile.close()

  check_diff(rows, rows_answer)


def check_zeus (outdir):
  csvfile = open(os.path.join(os.getenv("HOME"), "VeriSmart/scripts/sp20-script/zeus-answer.csv"), 'r')
  reader = csv.DictReader(csvfile)
  rows_answer = list(reader)
  csvfile.close()

  cmd = ["python2", os.path.join(os.getenv("HOME"), "VeriSmart/scripts/sp20-script/run.py"),
         os.path.join(os.getenv("HOME"), "VeriSmart/scripts/sp20-script/zeus-info.csv"),
         os.path.join(os.getenv("HOME"), "VeriSmart-old/benchmark/zeus/deduplicated"),
         outdir]

  p = subprocess.Popen (cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
  stdout = p.stdout.read()
  p.wait()
  p.poll()
  # print(stdout.decode("utf-8"))

  csvfile = open (os.path.join (outdir, "summary.csv"), 'r')
  reader = csv.DictReader(csvfile)
  rows = list (reader)
  csvfile.close()

  check_diff(rows, rows_answer)


def main ():
  outdir = os.path.join(os.getcwd(), datetime.now().strftime ('%y%m%d-%H%M%S'))
  outdir_cve = os.path.join(outdir,"cve")
  outdir_zeus = os.path.join(outdir,"zeus")

  os.mkdir(outdir)
  os.mkdir(outdir_cve)
  os.mkdir(outdir_zeus)
  
  print('Checking cve data set ...')
  check_cve(outdir_cve)

  print ('\n')

  print ('Checking zeus data set ...')
  check_zeus(outdir_zeus)


if __name__ == "__main__":
  main ()
