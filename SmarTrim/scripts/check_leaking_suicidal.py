import os
import csv
import argparse 
import json
from os.path import expanduser

HOME = expanduser("~")
LABEL = os.path.join (HOME, 'benchmarks/labels/leaking_suicidal_labels.json')
OUTFILE = 'leaking_suicidal_mark.csv'
FIELD = ['file', 'error',
         'leaking_target', 'leaking_target_detected', 'leaking_report',
         'suicidal_target', 'suicidal_target_detected', 'suicidal_report'
        ]

def collect_per (directory, fname):
  with open (os.path.join(directory, fname), 'r') as fp:
    json_data = json.load(fp)

  vuls = {'leaking': [] , 'suicidal': []}
  for j in json_data['result']:
    if j['kind'] == "ETH_LEAK":
      vuls['leaking'].append(j['line'])
    elif j['kind'] == "KA":
      vuls['suicidal'].append(j['line'])
    else:
      pass

  assert (len (vuls['leaking']) == len (set(vuls['leaking'])))
  assert (len (vuls['suicidal']) == len (set(vuls['suicidal'])))
  
  vuls['leaking'] = sorted(vuls['leaking'])
  vuls['suicidal'] = sorted(vuls['suicidal'])
  return vuls

def collect (directory):
  json_list = [f for f in os.listdir(directory) if f.endswith('.json')]
  txt_list = [f for f in os.listdir(directory) if f.endswith('.txt') and f!='cmd_history.txt' and f!='took.txt']

  res = []
  for (i,txt) in enumerate(txt_list):
    json_file = txt.replace('.txt', '.json')
    fname = txt.replace('.txt', '.sol')
    if json_file in json_list:
      per = collect_per (directory, json_file)
      res.append ({'file': fname, 'error':'', 'leaking': per['leaking'], 'suicidal': per['suicidal']})
    else: # json file is not generated due to some errors
      res.append ({'file': fname, 'error':'O', 'leaking': [], 'suicidal': []})
  return res

def check(reports, answers):
  marks = []
  for report in reports:
    ans = list (filter (lambda a: report['file'] == a['file'], answers))
    assert (len(ans) == 1)
    ans = ans[0]
  
    fname = report['file']
    marks.append ({
      'file': fname,
      'error': report['error'],
      'leaking_target': len(ans['leaking']),
      'leaking_target_detected': len (set(ans['leaking']).intersection(set(report['leaking']))),
      'leaking_report': len(report['leaking']),
      'suicidal_target': len(ans['suicidal']),
      'suicidal_target_detected': len (set(ans['suicidal']).intersection(set(report['suicidal']))),
      'suicidal_report': len(report['suicidal'])
    })
  return marks

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument ('--d', type=str)

  args = parser.parse_args()

  reports = collect (args.d)
  with open (LABEL,'r') as fp:
    labs = json.load(fp)
  marks = check (reports, labs)


  with open(os.path.join(args.d, OUTFILE), 'w') as fp:
    writer = csv.DictWriter(fp, fieldnames=FIELD)
    writer.writeheader()
    writer.writerows(marks)

if __name__ == "__main__":
  main()
