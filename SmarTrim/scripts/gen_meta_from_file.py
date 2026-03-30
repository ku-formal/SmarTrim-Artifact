import os
import argparse 
from datetime import datetime
import json
import csv
import requests
from time import sleep

field = ["id", "main_name", "loc", "compiler_version"]

def find_solc_main_from_addr (addr):
  # sleep(0.1)
  key = "38P32GDPKMNGAYBXVHQGWXTYP8FE8BF4T9"
  src_url = "https://api.etherscan.io/api?module=contract&action=getsourcecode&address=" + addr + "&apikey=" + key
  response = requests.get(src_url)
  content = response.content
  j = json.loads (content)

  main_name = j["result"][0]["ContractName"].encode('utf8').decode('utf8')
  solc_ver = j["result"][0]["CompilerVersion"].encode('utf8').decode('utf8')

  return (solc_ver, main_name)


def find_between_string (s, start, end):
  n1 = s.index(start)
  n2 = s.index(end)
  return s[n1+1:n2]


def find_solc_from_file (lines):
  encountered = 0
  solc = ""
  for line in lines:
    if 'pragma solidity ^' in line:
      solc = find_between_string (line, '^', ';')
      encountered += 1
    elif 'pragma solidity ' in line:
      solc = line.strip().replace("pragma solidity ","").replace(";","")
      encountered += 1
    else:
      pass

  if encountered == 1:
    solc = 'v' + solc + '+'
  elif encountered == 0:
    solc = "WARNING:no_pragma_exist or unexpected syntax exists"
  else:
    solc = "WARNING:multiple_pramga_exist"
  return solc


def find_main_from_file (lines):
  for (i,line) in enumerate(lines):
    if 'contract' in line and ('{' in line or '{' in lines[i+1]): # the string 'contract' may exist in comments
      return line.strip().replace("contract ","").replace ("{","").replace(" ","")
  return "WARNING:invalid_contract_name"


def find_solc_main (filename,lines):
  if filename.startswith("0x"):
    return find_solc_main_from_addr (filename.replace(".sol",""))
  else:
    return (find_solc_from_file(lines), find_main_from_file (lines))


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument ('--dir', type=str)

  args = parser.parse_args()

  json_list = [f for f in os.listdir(args.dir) if f.endswith(".sol")]
  fp_meta = open ("meta.csv", 'w')
  writer = csv.DictWriter(fp_meta, fieldnames=field)
  writer.writeheader()
  
  for filename in json_list:
    fp = open (os.path.join(args.dir, filename), 'r')

    lines = fp.readlines()
    (solc,contract_name) = find_solc_main (filename,lines)
    total_lines = len(lines)
    writer.writerow({
      'id': filename.replace(".sol", ""),
      'main_name': contract_name,
      'loc': total_lines,
      'compiler_version': solc
    })

    fp.close()

  fp_meta.close()

if __name__ == "__main__":
  main()
