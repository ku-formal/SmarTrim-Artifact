import os
import argparse 
from datetime import datetime
import json
import csv

fieldnames_query = ["qid", "fid", "kind", "depth", "disproven_time", "raw", "raw2"]

fieldnames_file = ["fname", "error", "time",
                   "iter", "last-disproven-iter", "last-disproven-time",
                   "max-explored-depth", "max-disproven-depth", "formulas", "timeout-formulas", "exit-covered",

                   "queries", "disproven",
                   "IO", "IO_depth1", "IO_depth2", "IO_depth3", "IO_deeper",
                   "DZ", "DZ_depth1", "DZ_depth2", "DZ_depth3", "DZ_deeper",
                   "ASSERT", "ASSERT_depth1", "ASSERT_depth2", "ASSERT_depth3", "ASSERT_deeper",
                   "ERC20", "ERC20_depth1", "ERC20_depth2", "ERC20_depth3", "ERC20_deeper",
                   "KA", "KA_depth1", "KA_depth2", "KA_depth3", "KA_deeper",
                   "LEAK", "LEAK_depth1", "LEAK_depth2", "LEAK_depth3", "LEAK_deeper",

                   "transfer", "transferFrom", "approve", "balance", "allowance", "totalSupply"]

def empty_fields():
  return dict.fromkeys(fieldnames_file, '')
  
#TODO: https://stackoverflow.com/questions/1871524/how-can-i-convert-json-to-csv
def per_query (json_data):
  tmp = []
  if 'result' not in json_data:
    return []
  for j in json_data['result']:
    tmp.append({
      'qid': json_data['baseName'].replace(".sol", "") + '_' + j['no'].zfill(2),
      'fid': json_data['baseName'].replace('.sol',''),
      'kind': j['kind'],
      'depth': j['depth'],
      'disproven_time': j['disprovenTime'],
      'raw': j['raw'],
      'raw2': j['raw2']
    })
  return tmp

def per_file (directory, fname):
  with open (os.path.join(directory, fname), 'r') as fp:
    json_data = json.load(fp)
  if json_data['errMsg'] != None:
    tmp = empty_fields()
    tmp['fname'] = json_data['baseName']
    tmp['error'] = json_data['errMsg']
    tmp['time'] = json_data['time']
    return tmp

  s = json_data['summary']
  tmp = {
    'fname': json_data['baseName'],
    'error': "" if json_data['errMsg'] == None else json_data['errMsg'],
    'time': json_data['time'],

    'iter': s['iter'],
    'last-disproven-iter': s['last-disproven-iter'],
    'last-disproven-time': s['last-disproven-time'],
    'max-explored-depth': s['max-explored-depth'],
    'max-disproven-depth': s['max-disproven-depth'],
    'formulas': s['formulas'],
    'timeout-formulas': s['timeout-formulas'],
    'exit-covered': s['exit-covered'],
    
    'queries': s['queries'],
    'disproven': s['disproven'],
    'IO': s['IO'], 'IO_depth1': s['IO_depth1'], 'IO_depth2': s['IO_depth2'], 'IO_depth3': s['IO_depth3'], 'IO_deeper': s['IO_deeper'],
    'DZ': s['DZ'], 'DZ_depth1': s['DZ_depth1'], 'DZ_depth2': s['DZ_depth2'], 'DZ_depth3': s['DZ_depth3'], 'DZ_deeper': s['DZ_deeper'],
    'ASSERT': s['ASSERT'],  'ASSERT_depth1': s['ASSERT_depth1'], 'ASSERT_depth2': s['ASSERT_depth2'],
    'ASSERT_depth3': s['ASSERT_depth3'], 'ASSERT_deeper': s['ASSERT_deeper'],
    'ERC20': s['ERC20'], 'ERC20_depth1': s['ERC20_depth1'], 'ERC20_depth2': s['ERC20_depth2'], 'ERC20_depth3': s['ERC20_depth3'], 'ERC20_deeper': s['ERC20_deeper'],
    'KA': s['KA'], 'KA_depth1': s['KA_depth1'], 'KA_depth2': s['KA_depth2'],
    'KA_depth3': s['KA_depth3'], 'KA_deeper': s['KA_deeper'],
    'LEAK': s['ETH_LEAK'], 'LEAK_depth1': s['ETH_LEAK_depth1'], 'LEAK_depth2': s['ETH_LEAK_depth2'], 'LEAK_depth3': s['ETH_LEAK_depth3'], 'LEAK_deeper': s['ETH_LEAK_deeper'],

    'transfer': s['transfer_deviated'],
    'transferFrom': s['transferFrom_deviated'],
    'approve': s['approve_deviated'],
    'balance': s['balance_deviated'],
    'allowance': s['allowance_deviated'],
    'totalSupply': s['total_deviated']
  }
  return tmp


def main():
  parser = argparse.ArgumentParser(epilog='python gen_summary.py --d INPUT_DIR')
  parser.add_argument ('--d', type=str)
  args = parser.parse_args ()

  json_list = [f for f in os.listdir(args.d) if f.endswith('.json')]
  txt_list = [f for f in os.listdir(args.d) if f.endswith('.txt') and f!='cmd_history.txt' and f!='took.txt']
  
  print(json_list)

  queries = []
  summaries = []
  for txt in txt_list:
    json_file = txt.replace('.txt','.json')
    if json_file in json_list:
      with open (os.path.join (args.d, json_file), 'r') as fp:
        json_data = json.load(fp)
      tmp = per_query(json_data)
      queries = queries + tmp
      per = per_file(args.d, json_file)
      summaries.append(per)
    else: # json file is not generated, e.g., memory error
      per = empty_fields()
      per['fname'] = txt.replace('.txt','.sol')
      per['error'] = 'no json'
      summaries.append(per)

  with open(os.path.join(args.d, 'res_per_query.csv'), 'w') as fp:
    data = sorted(queries, key=lambda row: float(row['disproven_time']))
    assert (len(queries) == len(data))
    writer = csv.DictWriter(fp, fieldnames=fieldnames_query)
    writer.writeheader()
    writer.writerows(data)

  for s in summaries:
    assert (len(list(s.keys())) ==  len(fieldnames_file))

  with open(os.path.join(args.d, 'summary.csv'), 'w') as fp:
    writer = csv.DictWriter(fp, fieldnames=fieldnames_file)
    writer.writeheader()
    writer.writerows(summaries)


if __name__ == "__main__":
  main()
