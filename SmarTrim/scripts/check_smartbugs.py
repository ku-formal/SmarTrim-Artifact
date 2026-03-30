import os
import sys
import tempfile
import time
import csv
import subprocess
from datetime import datetime
import argparse 
import json
from os.path import expanduser

HOME = expanduser("~")
ANSWER = os.path.join (HOME, 'smartbugs/dataset/vulnerabilities.json')
OUTPUT_NAME = "smartbugs_format.json"
FIELD = ['file',
         'reent_ratio', 'reent_target', 'reent_target_detected', 'reent_report',
         'access_ratio', 'access_target', 'access_target_detected', 'access_report',
         'unchecked_ratio', 'unchecked_target', 'unchecked_target_detected', 'unchecked_report'
        ]


def map_to_smartbugs (s):
  if s == "IO":
    return "arithmetic"
  elif s == "OA" or s == "succAnyone" or s == "killAnyone" or s == "Leaking":
    return "access_control"
  elif s == "RE_ENT": # XXX: fix SMARTEST output format
    return "reentrancy"
  elif s == "UNCHECKED_RET": # XXX: fix SMARTEST output format
    return "unchecked_low_level_calls"
  else:
    return s

def process_single(directory, json_file, acc):
  fp = open (os.path.join(directory, json_file), 'r')
  json_data = json.load(fp)
  fp.close()

  vuls = []
  for j in json_data['result']:
    vuls.append({
      'category': map_to_smartbugs(j['kind']),
      'lines': [int(j['line'])]
    })

  acc.append({
    'name': json_data['baseName'],
    'vulnerabilities': vuls
  })



def process_multi(directory):
  json_files = [f for f in os.listdir(directory) if f.endswith(".json") and f!=OUTPUT_NAME]
  
  acc = []
  for json_file in json_files:
    process_single(directory, json_file, acc)
  
  with open (os.path.join(directory, OUTPUT_NAME), 'w') as fp:
    json.dump (acc, fp, indent=4)


def count_detected (vuls_res, vuls_ans, vulname):
  cnt = 0
  for a in vuls_ans:
    if a['category']!=vulname:
      continue
    for r in vuls_res:
      b1 = set(r['lines']).issubset(set(a['lines']))
      assert (r['lines']!=0 and a['lines']!=0)
      b2 = a['category'] == r['category']
      cnt = cnt+1 if (b1 and b2) else cnt
  return cnt


def count_all (vuls, vulname):
  cnt = 0
  for v in vuls:
    cnt = cnt+1 if v['category']==vulname else cnt
  return cnt


def compute_ratio (detect_num, target_num):
  assert (target_num >= detect_num)
  if target_num==0:
    return "N/A"
  else:
    return detect_num / target_num


def check(directory, res_list, answer_list):
  fp = open (os.path.join(directory,'smartbugs_result.csv'), 'w')
  writer = csv.DictWriter(fp, fieldnames=FIELD)
  writer.writeheader()

  for res in res_list:
    ans = list (filter (lambda a: res['name'] == a['name'], answer_list))
    # assert (len(ans) == 1) # duplicated contract: 0x627fa62ccbb1c1b04ffaecd72a53e37fc0e17839.sol (Reentrancy and access_control)
    assert (len(ans)>=1)

    vuls_ans = []
    for a in ans: # merge vulnerabilities in answer set data for duplicated contracts
      vuls_ans += a['vulnerabilities']
    vuls_res = res['vulnerabilities']
  
    reent_report = count_all (vuls_res, 'reentrancy')
    reent_target = count_all (vuls_ans, 'reentrancy')
    reent_detected = count_detected (vuls_res, vuls_ans, 'reentrancy')
    reent_ratio = compute_ratio (reent_detected, reent_target)

    access_report = count_all (vuls_res, 'access_control')
    access_target = count_all (vuls_ans, 'access_control')
    access_detected = count_detected (vuls_res, vuls_ans, 'access_control')
    access_ratio = compute_ratio (access_detected, access_target)

    unchecked_report = count_all (vuls_res, 'unchecked_low_level_calls')
    unchecked_target = count_all (vuls_ans, 'unchecked_low_level_calls')
    unchecked_detected = count_detected (vuls_res, vuls_ans, 'unchecked_low_level_calls')
    unchecked_ratio = compute_ratio (unchecked_detected, unchecked_target)


    writer.writerow({
      'file': res['name'],

      'reent_ratio': reent_ratio,
      'reent_target': reent_target,
      'reent_target_detected': reent_detected,
      'reent_report': reent_report,

      'access_ratio': access_ratio,
      'access_target': access_target,
      'access_target_detected': access_detected,
      'access_report': access_report,

      'unchecked_ratio': unchecked_ratio,
      'unchecked_target': unchecked_target,
      'unchecked_target_detected': unchecked_detected,
      'unchecked_report': unchecked_report
    })
  fp.close()

def main():
  parser = argparse.ArgumentParser()

  parser.add_argument ('--dir', type=str)
  parser.add_argument ('--target', type=str, nargs='+', default='all')

  args = parser.parse_args()
  
  process_multi (args.dir) # generate analysis summaries in smartbugs' json format

  with open (os.path.join(args.dir, OUTPUT_NAME)) as f:
    res_list = json.load(f)
  with open (ANSWER, 'r') as f:
    answer_list = json.load(f)

  check (args.dir, res_list, answer_list)

if __name__ == "__main__":
  main()
