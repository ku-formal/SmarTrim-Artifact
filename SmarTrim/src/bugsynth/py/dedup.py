import csv
import time
import argparse
import os
from pprint import pprint as pp

fieldnames = ['id', 'duplicate_of', 'actual_order', 'main_name', 'loc', 'address', 'compiler_version', 'original_compiler_version', 'is_multiple', 'fail']

def exact_match_src (fname1, fname2):
    fp1 = open(fname1)
    fp2 = open(fname2)
    s1 = fp1.read()
    s2 = fp2.read()
    fp1.close()
    fp2.close()
    return s1 == s2

def count_dup(dict_list, pgmdir):
    label = 0
    final_map = dict()
    dup_map = dict()
    # first_map = dict()

    total = len(dict_list)
    for (i,row) in enumerate (dict_list):
      print(f'processing {i+1}/{total} ... ' + os.path.join(pgmdir, row['id'] + '.sol')) # may not exist if not collected properly.
      b1 = os.path.isfile(os.path.join(pgmdir, row['id'] + '.sol'))
      b2 = not (row['id'] in dup_map) # not a duplicate of previously explored ones
      b3 = not(row['compiler_version'].startswith('vyper')) # not a vyper contract
      if b1 and b2 and b3:
        label += 1 # fresh label
        final_map[row['id']] = str(label)
        # dup_map[row['id']] = row['id']

        # see if there exist duplicate of i-th contract -- for some j (such that j > i)
        for (j,row2) in enumerate (dict_list):
          # assert (os.path.isfile (os.path.join (pgmdir, row2['id'] + '.sol')))
          exist_file = os.path.isfile (os.path.join (pgmdir, row2['id'] + '.sol'))
          if exist_file and j > i:
            fname1 = os.path.join (pgmdir, row['id'] + '.sol')
            fname2 = os.path.join (pgmdir, row2['id'] + '.sol')
            if exact_match_src(fname1, fname2) and row['main_name'] == row2['main_name']:
              # print (str(i) + ', ' + str(j))
              dup_map[row2['id']] = row['id']

    return (dup_map, final_map)

def main ():
    parser = argparse.ArgumentParser(epilog='python3 dedup.py --root_info {ROOT_DIR} --pgmdir {PGM_DIR}')
    parser.add_argument ('--root_info', type=str, help='E.g., raw/meta.csv')
    parser.add_argument ('--pgmdir', type=str, help='directory containing .sol files')
    parser.add_argument ('--out', type=str, help='output csv file name', default='deduplicated.csv')

    args = parser.parse_args ()

    csvfile = open (args.root_info, 'r')
    reader = csv.DictReader (csvfile)
    dict_list = list(reader) # when reader is iterated once, seems no more reading is available.
    csvfile.close()

    before = time.time()
    dup_map, final_map = count_dup (dict_list, args.pgmdir)
    after = time.time()

    print ('Took: ' + str(after-before))
    # pp (dup_map)

    outcsv = args.out
    fp_out = open (outcsv, 'w')
    writer = csv.DictWriter(fp_out, fieldnames=fieldnames, lineterminator=os.linesep)
    writer.writeheader()

    for row in dict_list:
      writer.writerow({
        'id': row['id'],
        'duplicate_of': dup_map.get (row['id'], ''),
        'actual_order': final_map.get (row['id'], ''),
        'main_name': row['main_name'],
        'loc': row['loc'],
        'address': row['address'],
        'compiler_version': row['compiler_version'],
        'original_compiler_version': row.get('original_compiler_version', row['compiler_version']),
        'is_multiple': row['is_multiple'],
        'fail': row['fail']
      })

    fp_out.close()

    print ('csv generated at : ' + os.path.join(os.getcwd(), outcsv))

if __name__ == '__main__':
    main ()
