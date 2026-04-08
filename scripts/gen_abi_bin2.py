"""
Similar as `gen_abi_bin.py` but use solc from solc-select.
Useful for smartian, slise, etc
"""

import os
import argparse
import csv
import subprocess
import utils
from utils import solc_select_path, BENCH_DIR
from multiprocessing import Pool

HOME = os.path.expanduser('~')
COMPILE_FOR = 'smartian'

def min_solv() -> str:
    if COMPILE_FOR == 'smartian':
        return "0.4.17"
    return "0.0.0"

# genenerate bytecode
def gen_bc(full_fname, solv: str, main_name, ext, outdir):
    flag = '--bin-runtime' if ext == 'bin_runtime' else '--bin'
    cmd = [solc_select_path(solv), full_fname, flag]
    try:
        result = subprocess.run(cmd, capture_output=True, check=True)
    except subprocess.CalledProcessError as e:
        print(e.stderr)
        raise e
    cmdout = result.stdout.decode('utf-8')
    cmderr = result.stderr.decode('utf-8')
    if result.returncode != 0:
        name = full_fname.split('/')[-1]
        with open(f'./.logs/{name}.log', 'w') as fp:
            fp.write(cmderr)
        assert False

    bar7 = '======='
    idx = cmdout.index(bar7) # Find the initial signal
    cmdout = cmdout[idx:] # preprocessing for handling warning messages
    lst = cmdout.split('\n\n')
    
    start = bar7 + ' ' + full_fname + ':' + main_name + ' ' + bar7
    lst = list(map(lambda x : x[1:] if x.startswith('\n=') else x, lst)) 
    # may start with '\n=' due to prepositive abstract contracts
    lst = list(filter(lambda x : x.startswith(start), lst))
    assert(len(lst) == 1)
    target = lst[0]
    target = target[:-1] if target[-1] == '\n' else target # remove '\n' at the end

    bin_just_before = target.rfind('\n')
    final = target[bin_just_before+1:]
    assert(len(final) > 0)
    
    name = full_fname.split('/')[-1].replace('.sol', '')
    with open(os.path.join(outdir, f'{name}.{ext}'), 'w') as fp:
        fp.write(final)

    return None


def gen_abi(full_fname, solv, main_name, outdir):
    ext = 'abi'
    cmd = [solc_select_path(solv), full_fname, '--abi']
    out = subprocess.run(cmd, capture_output=True, check=True)
    cmdout = out.stdout.decode('utf-8')
    idx = cmdout.index('=======') # Find the initial signal
    cmdout = cmdout[idx:] # preprocessing for handling warning messages

    lst = cmdout.split('\n\n')
   
    start = '======= ' + full_fname + ':' + main_name + ' =======\n'
    lst = list(filter(lambda x: x.startswith(start), lst))
    assert (len(lst) == 1)
    target = lst[0]
    target = target[:-1] if target[-1] == '\n' else target # remove '\n' at the end

    j = target.rfind('\n')
    final = target[j+1:]
    assert(len(final) > 0)
    
    name = full_fname.split('/')[-1].replace('.sol', '')
    with open(os.path.join(outdir, f'{name}.{ext}'), 'w') as fp:
        fp.write(final)

    return None

def get_result(full_fname, solv, main_name, ext, outdir):
    # if COMPILE_FOR == 'prettysmart':
    #     if ext != 'hex':
    #         return None
    #     return gen_all_bc(full_fname, solv, ext, outdir)
    if ext == 'abi':
        res = gen_abi(full_fname, solv, main_name, outdir)
        return res
    elif ext == 'bin' or ext == 'hex' or ext == 'bin_runtime':
        res = gen_bc(full_fname, solv, main_name, ext, outdir)
        return res
    else:
        assert False

def run_each(i, row, pgmdir, outdir, ext) -> str | None:
    """
    :returns None if the cmd succeed, else returns that failed cmd as a string
    """
    solv = utils.get_solv(row, minimum_required=min_solv())
    main_name = row['main_name']
    fid = row['id']
    full_fname = os.path.join(pgmdir, fid) + '.sol'
    outfile_path = os.path.join(outdir, fid) + f'.{ext}'

    print(f'processing {i}: {outfile_path}')
    
    try:
        res = get_result(full_fname, solv, main_name, ext, outdir)
        assert res is None
        return None
    except Exception as e:
        print(e)
        print('Failed : ', full_fname, solv, ext)
        return " ".join([full_fname, solv, ext])


def loop(rows, pgmdir, outdir, ext, proc):
    # if ext == 'bin' and COMPILE_FOR == 'prettysmart':
    #     ext = 'hex'
    pool = Pool(proc)
    args_list = [(i, rows[i], pgmdir, outdir, ext) for i in range(len(rows))]
    l : list[str | None] = pool.starmap(run_each, args_list)
    pool.close()
    pool.join() # need this? idk
    failed_cmds : list[str] = [x for x in l if x is not None]
    return failed_cmds


def get_tasks(root_info):
    fp = open(root_info, 'r')
    rows = list(csv.DictReader(fp))
    rows = list(filter(lambda row: row['actual_order'] != '', rows))
    assert(len(rows) > 0)
    fp.close()

    return rows

def main():
    parser = argparse.ArgumentParser(epilog='run solc to generate abi/bin/bin-runtime files')
    parser.add_argument('--compile-for', type=str, default='smartian')
    parser.add_argument('-d', '--dataset', type=str)
    parser.add_argument('--root-info', type=str, default='')
    parser.add_argument('--pgmdir', type=str, default=f'')
    parser.add_argument('--process', type=int, default=24)

    args = parser.parse_args()
    if args.root_info == '': # default
        args.root_info = f'{BENCH_DIR}/meta/{args.dataset}.csv'
    if args.pgmdir == '': # default
        args.pgmdir = f'{BENCH_DIR}/contracts/{args.dataset}'
    print(args)
    
    global COMPILE_FOR
    COMPILE_FOR = args.compile_for
    
    rows = get_tasks(args.root_info)

    pgmdir = args.pgmdir
    pgmdir = pgmdir[:-1] if pgmdir.endswith('/') else pgmdir # '.../dir/' => '.../dir'
    outdir_abi = f'./for-{COMPILE_FOR}/{args.dataset}.abi'
    outdir_bin = f'./for-{COMPILE_FOR}/{args.dataset}.bin'
    outdir_bin_runtime = f'./for-{COMPILE_FOR}/{args.dataset}.bin-runtime'

    os.makedirs(outdir_abi, exist_ok=True)
    os.makedirs(outdir_bin, exist_ok=True)
    os.makedirs(outdir_bin_runtime, exist_ok=True)
    os.system('rm -fr ./.logs/*')
    os.makedirs("./.logs", exist_ok=True)

    proc = args.process
    
    abi_fail = loop(rows, pgmdir, outdir_abi, 'abi', proc)
    print('')
    bin_fail = loop(rows, pgmdir, outdir_bin, 'bin', proc)
    print('')
    bin_run_fail = loop(rows, pgmdir, outdir_bin_runtime, 'bin_runtime', proc)
    print('')
    
    print('Done.')
    
    if len(abi_fail) + len(bin_fail) + len(bin_run_fail) > 0:
        print('')
        print('=== failed list ===')
        print('\n'.join(abi_fail))
        print('\n'.join(bin_fail))
        print('\n'.join(bin_run_fail))
        exit(75)
        
    exit(0)


if __name__ == "__main__":
    main()
