import sys
import os
import argparse
import csv
import subprocess
# import basic_utils
from multiprocessing import Pool, Lock, Value, Manager

LOCK = Lock ()
cnt = Value ('i', 0)
TOTAL_NUM = 0

fail_cmd_lst = Manager().list()

# genenerate bytecode
def gen_bc(full_fname, solv, main_name, is_runtime_bc):
    flag = '--bin-runtime' if is_runtime_bc else '--bin'
    cmd = ['solc_' + solv, full_fname, flag]
    out = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
    cmdout = out.stdout.read()
    cmdout = cmdout.decode ('utf-8')

    bar7 = '======='
    idx = cmdout.index (bar7) # Find the initial signal
    cmdout = cmdout[idx:] # preprocessing for handling warning messages
    lst = cmdout.split('\n\n')
    # print(lst)
    # assert(False)
    start = bar7 + ' ' + full_fname + ':' + main_name + ' ' + bar7
    lst = list (map (lambda x : x[1:] if x.startswith('\n=') else x, lst)) # may start with '\n=' due to prepositive abstract contracts
    lst = list (filter (lambda x : x.startswith(start), lst))
    assert (len (lst) == 1)
    target = lst[0]
    target = target[:-1] if target[-1] == '\n' else target # remove '\n' at the end

    bin_just_before = target.rfind('\n')
    final = target[bin_just_before+1:]
    assert(len(final) > 0)

    return final


def gen_abi(full_fname, solv, main_name):
    cmd = ['solc_' + solv, full_fname, '--abi']
    out = subprocess.Popen (cmd, stdout= subprocess.PIPE, stderr=subprocess.STDOUT)
    cmdout = out.stdout.read()
    cmdout = cmdout.decode ('utf-8')
    idx = cmdout.index ('=======') # Find the initial signal
    cmdout = cmdout[idx:] # preprocessing for handling warning messages
    # print ('==PREPROCESSING===')
    # print(cmdout)

    lst = cmdout.split ('\n\n')
   
    start = '======= ' + full_fname + ':' + main_name + ' =======\n'
    lst = list (filter (lambda x : x.startswith (start), lst))
    assert (len (lst) == 1)
    target = lst[0]
    target = target[:-1] if target[-1] == '\n' else target # remove '\n' at the end

    j = target.rfind('\n')
    final = target[j+1:]
    assert(len(final) > 0)

    return final

def get_cmd(full_fname, solv, main_name, ext):
    if ext == 'abi':
        return ['solc_' + solv, full_fname, '--abi']
    elif ext == 'bin':
        return ['solc_' + solv, full_fname, '--bin']
    elif ext == 'bin_runtime':
        return ['solc_' + solv, full_fname, '--bin-runtime']
    else:
        assert(False)

def get_result(full_fname, solv, main_name, ext):
    if ext == 'abi':
        res = gen_abi (full_fname, solv, main_name)
        return res
    elif ext == 'bin':
        res = gen_bc (full_fname, solv, main_name, False)
        return res
    elif ext == 'bin_runtime':
        res = gen_bc (full_fname, solv, main_name, True)
        return res
    else:
        assert (False)

def run_each (row, pgmdir, outdir, ext):
    solv = basic_utils.get_solv(row)
    main_name = row['main_name']
    fid = row['id']
    full_fname = os.path.join(pgmdir,fid) + '.sol'
    outfile_path = os.path.join(outdir,fid) + f'.{ext}'

    LOCK.acquire()
    cnt.value += 1
    print ('processing ' + str(cnt.value) + '/' + str(TOTAL_NUM) + ' ... ' + f'{outfile_path}')
    LOCK.release()

    try:
        res = get_result (full_fname, solv, main_name, ext)
        with open (outfile_path, 'w') as fp:
          fp.write(res)
        # print(str(i+1) + '/' + str(len(rows)) + ' ... ' + f'{outfile_path}' + ' generated.')
    except:
        LOCK.acquire()
        cmd = get_cmd(full_fname, solv, main_name, ext)
        fail_cmd_lst.append(' '.join(cmd))
        print ('Failed : ' + ' '.join(cmd))
        # print(str(i+1) + '/' + str(len(rows)) + ' ... ' + f'{outfile_path}' + ' failed!!!!!')
        LOCK.release()


def loop(rows, pgmdir, outdir, ext, proc):
    pool = Pool(proc)
    args_lst = list(map(lambda r: (r, pgmdir, outdir, ext), rows))
    pool.starmap(run_each, args_lst)
    pool.close()
    pool.join()


def get_tasks (root_info):
    fp = open(root_info, 'r')
    rows = list(csv.DictReader(fp))
    rows = list (filter (lambda row: row['actual_order'] != '', rows))
    assert(len(rows) > 0)
    fp.close()

    global TOTAL_NUM
    TOTAL_NUM = len(rows)

    return rows

def main():
    parser = argparse.ArgumentParser(epilog = 'do setup for Smartian tool')
    parser.add_argument ('--root_info', type=str)
    parser.add_argument ('--pgmdir', type=str)
    parser.add_argument ('--process', type=int, default=24)

    args = parser.parse_args()

    rows = get_tasks (args.root_info)

    pgmdir = args.pgmdir
    pgmdir = pgmdir[:-1] if pgmdir.endswith ('/') else pgmdir # '.../dir/' => '.../dir'
    # assert(pgmdir.endswith('/sol'))
    outdir_abi = pgmdir + '_abi' # '.../dir => .../dir_abi
    outdir_bin = pgmdir + '_bin' # '.../dir => .../dir_bin
    outdir_bin_runtime = pgmdir + '_bin_runtime' # '.../dir => .../dir_bin_runtime

    rm_abi = f'rm -rf {outdir_abi}'
    mk_abi = f'mkdir  {outdir_abi}'
    rm_bin = f'rm -rf {outdir_bin}'
    mk_bin = f'mkdir  {outdir_bin}'
    rm_bin_runtime = f'rm -rf {outdir_bin_runtime}'
    mk_bin_runtime = f'mkdir  {outdir_bin_runtime}'

    assert(os.system(rm_abi) == 0)
    print(f'command executed successfully: {rm_abi}')

    assert(os.system(mk_abi) == 0)
    print(f'command executed successfully: {mk_abi}')

    assert(os.system(rm_bin) == 0)
    print(f'command executed successfully: {rm_bin}')

    assert(os.system(mk_bin) == 0)
    print(f'command executed successfully: {mk_bin}')
    print('')

    assert(os.system(rm_bin_runtime) == 0)
    print(f'command executed successfully: {rm_bin_runtime}')

    assert(os.system(mk_bin_runtime) == 0)
    print(f'command executed successfully: {mk_bin_runtime}')
    print('')

    proc = args.process
    loop(rows, pgmdir, outdir_abi, 'abi', proc)

    cnt.value = 0
    print('')
    loop(rows, pgmdir, outdir_bin, 'bin', proc)

    cnt.value = 0
    print('')
    loop(rows, pgmdir, outdir_bin_runtime, 'bin_runtime', proc)

    print('')
    print('Done.')

    print('')
    print('=== failed list ===')
    print('\n'.join(fail_cmd_lst))


if __name__ == "__main__":
    main()
