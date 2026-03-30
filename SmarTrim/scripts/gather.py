import os
import subprocess
import time
from os.path import expanduser
import sys
from datetime import datetime
import argparse 

def main():
  parser = argparse.ArgumentParser(description='gather files from aggregated directory')

  parser.add_argument ('--src', type=str, help='directory with aggreagted files')
  parser.add_argument ('--dst', type=str, help='destination directory')
  parser.add_argument ('--include', type=str, default='', help='.txt file with program lists to be included')
  args = parser.parse_args ()

  src = args.src
  dst = args.dst
  include = args.include

  include_list = []
  with open(include, 'r') as fp:
    include_list = [line.replace('\r\n','').replace ('\n','') for line in fp]

  exclude = ['cmd_history.txt', 'res_per_query.csv', 'summary.csv', 'time.csv', 'took.csv']
  files = os.listdir(src)

  files = list(filter(lambda f: not (f in exclude), files))
  files = list(filter(lambda f: f.replace('.txt','').replace('.json','') in include_list, files))

  txt_list = list(filter(lambda f: f.endswith('.txt'), files))
  assert(len(txt_list) == len(include_list))

  for f in files:
    src_dir = os.path.join(src,f)
    dst_dir = os.path.join(dst,f)
    assert(os.system(f'cp {src_dir} {dst_dir}') == 0)

  assert(os.system(f'python3 ~/VeriSmart/scripts/gen_summary.py --d {dst}') == 0)


if __name__ == "__main__":
  main()
