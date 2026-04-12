"""
Execute tools.
"""

import argparse
import datetime
import json
import os
import re
import requests
import subprocess
import numpy as np
import pandas as pd
from multiprocessing import Pool
import platform

from utils import ARTIFACT_REPO, BENCH_DIR

starttime_datetime = datetime.datetime.now()
program = ''
jobs = 24
OUTPUT = f"{ARTIFACT_REPO}/output"

TIMEOUT = str(1800)
KILL_TIMEOUT = str(2100)
CONFUZZIUS_KILL_TIMEOUT = str(2400)
EFCF_KILL_TIMEOUT = str(3000)

def get_program_cwd():
    """
    working directory to run programs
    """
    return ARTIFACT_REPO
    
def set_globals(args: argparse.Namespace, config):
    global program
    global jobs
    global TIMEOUT
    global KILL_TIMEOUT
    global EFCF_KILL_TIMEOUT
    global CONFUZZIUS_KILL_TIMEOUT
    
    program = args.tool
        
    jobs_config = config['jobs']
    key = f"{args.dataset}:{args.tool}"
    if key not in jobs_config:
        jobs = jobs_config['default']
    else:
        jobs = jobs_config[key]
        
    if "timeout" in config:
        TIMEOUT = str(config['timeout'])
        
    if "kill_timeout" in config:
        KILL_TIMEOUT = str(config['kill_timeout'])
        if int(KILL_TIMEOUT) <= 20:
            EFCF_KILL_TIMEOUT = KILL_TIMEOUT
            CONFUZZIUS_KILL_TIMEOUT = KILL_TIMEOUT
        
    assert isinstance(jobs, int)
    

def build_outdir(l):
    for s in l:
        os.mkdir(os.path.join(OUTPUT, s))

def get_ver_from_metastr(s: str) -> str:
    version_pattern = r'v\d\.\d\.\d{1,2}'
    version = re.findall(version_pattern, s)
    if len(version) == 0: return '0.8.20'
    return version[0][1:]
    
def max_version(s1: str, s2: str) -> str:
    l1 = list(map(int, s1.split('.')))
    l2 = list(map(int, s2.split('.')))
    if l1 > l2:
        return s1
    else:
        return s2

def run_command(cmd: list[str], id: str, i, n):
    output_dir = f"{OUTPUT}/{id}"
    temp_stdout_path = f"{OUTPUT}/{id}.stdout.txt"
    temp_stderr_path = f"{OUTPUT}/{id}.stderr.txt"
    stdout_path = os.path.join(output_dir, ".stdout.txt")
    stderr_path = os.path.join(output_dir, ".stderr.txt")
    try:
        start = datetime.datetime.now()
        print(f"{start} {i}/{n}", flush=True)
        with open(temp_stdout_path, "wb") as f_stdout, open(temp_stderr_path, "wb") as f_stderr:
            result = subprocess.run(cmd, stdout=f_stdout, stderr=f_stderr, cwd=get_program_cwd())
        end = datetime.datetime.now()
        print(f'END {id} time:{end - start} ret:{result.returncode}', flush=True)
        os.rename(temp_stdout_path, stdout_path)
        os.rename(temp_stderr_path, stderr_path)
        return (id + ".sol", " ".join(cmd), start, end, result.returncode)
    except KeyboardInterrupt:
        raise KeyboardInterrupt
    
def get_command_smartrim(df_: pd.DataFrame, dataset: str, is_baseline=False, st='inc'):
    assert st in ['inc', 'random']
    df = df_.copy()
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.16')
        if dataset == 'io':
            mo = "io"
        elif dataset == 'ls':
            mo = "el,su"
        elif dataset == 're':
            mo = "re"
        
        cmd: list[str] = [
            "docker", "run", "--rm", "--init",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/root/VeriSmart/output",
            "--mount", f"type=bind,src={BENCH_DIR}/contracts/{dataset}/{id}.sol,dst=/root/{id}.sol,ro",
            "--volume", "smartrim-artifact-solc-select:/root/.solc-select:ro",
            "--workdir", "/root/VeriSmart",
            "--entrypoint", "timeout",
            "my-smartrim:fse26",
            "--kill-after=10", KILL_TIMEOUT,
            "/root/VeriSmart/main.exe",
            "exploit",
            f"/root/{id}.sol",
            "--main", f"{main_name}",
            "--solv", v,
            "--timeout", TIMEOUT,
            "--solver-timeout", "90000",
            "--report",
            "--kind", mo,
            "--validate",
        ]
        
        if is_baseline == True:
            cmd.append('--no-pruning')
            
        if st == 'random':
            cmd.extend(['--depth', str(5), '--strategy', 'urandom-pop'])
        
        commands.append((cmd, id))
        
    return commands
    
def get_command_rlf(df: pd.DataFrame, dataset: str):
    assert dataset == 'ls'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        
        cmd: list[str] = [
            "docker", "run", "--rm", "--init", 
            "--volume", f"{ARTIFACT_REPO}/for-rlf/{id}:/rlf/input", 
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/rlf/output",
            "--entrypoint", "timeout",
            "--workdir", "/rlf",
            "damonhero/rlf:latest",
            "--kill-after=10", KILL_TIMEOUT,
            "python3", "-m", "rlf", 
            "--proj", f"/rlf/input", "--contract", main_name, 
            "--fuzzer", "reinforcement", 
            "--reward", "cov+bugs", 
            "--mode", "test", 
            "--detect_bugs", "ls",  
            "--limit", "1000000", 
            "--output_path", f"/rlf/output",
            "--limit_time", TIMEOUT,
        ]
        
        commands.append((cmd, id))
        
    return commands

def get_command_smartest(df_: pd.DataFrame, dataset: str):
    assert dataset != 're'
    commands = []
    df = df_.copy()
    if dataset == 'io' or dataset == 'ls':
        df_fold = pd.read_csv(f'{ARTIFACT_REPO}/assets/smartest/fold/{dataset}.csv', dtype=np.object_)
        df = df.merge(df_fold, how='left', on='id')
    
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.16')
        
        m = []
        if dataset == 'io':
            m += ['io']
        elif dataset == 'ls':
            m += ['leak', 'kill']
        elif dataset == 'pb':
            m += ['io', 'erc20']
            
        fold = row['fold']
        if fold not in ['1', '2', '3', '4']:
            train = f"{ARTIFACT_REPO}/assets/smartest/model/{dataset}_all"
        else:
            train = f"{ARTIFACT_REPO}/assets/smartest/model/{dataset}_{fold}"
            
        cmd: list[str] = [
            "docker", "run", "--rm", "--init",
            "--volume", "smartrim-artifact-solc-select:/build/.solc-select",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/build/verismart/output",
            "--volume", f"{train}:/build/verismart/src/exploit/train:ro",
            "--mount", f"type=bind,src={BENCH_DIR}/contracts/{dataset}/{id}.sol,dst=/build/{id}.sol,ro",
            "--workdir", "/build/verismart",
            "my-smartest:fse26",
            "bash", "-c", " ".join([
                f"cp /build/.solc-select/artifacts/solc-{v}/solc-{v} /usr/local/bin/solc_{v}",
                "&&",
                "timeout", "--kill-after=10", KILL_TIMEOUT,
                "./main.native",
                "-input", f"/build/{id}.sol",
                "-mode", "exploit",
                "-z3timeout", "60000",
                "-exploit_timeout", TIMEOUT,
                "-main", main_name, 
                "-solv", v,
                "-ngram", "3", 
                "-refined_vcgen", 
                "-report"
            ] + m)
        ]
        commands.append((cmd, id))
        
    return commands

def get_command_smartian(df: pd.DataFrame, dataset: str):
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        
        commands.append(([
            "docker", "run", "--rm", "--init",
            "--volume", "smartrim-artifact-4smartian:/root/for-smartian:ro",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/root/output",
            "--entrypoint", "timeout",
            "my-smartian:fse26",
            "--kill-after=10", KILL_TIMEOUT,
            "dotnet",
            "/root/Smartian/build/Smartian.dll",
            "fuzz",
            "--verbose", "1",
            "--program", f"/root/for-smartian/{dataset}.bin/{id}.bin",
            "--abifile", f"/root/for-smartian/{dataset}.abi/{id}.abi",
            "--timelimit", TIMEOUT,
            "--outputdir", f"/root/output",
        ], id))
        
    return commands

def get_command_mythril(df: pd.DataFrame, dataset: str):
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.12')
        if dataset == 'io':
            mo = "IntegerArithmetics"
        elif dataset == 'ls':
            mo = "EtherThief,AccidentallyKillable"
        elif dataset == 're':
            mo = "StateChangeAfterCall"
        elif dataset == 'pb':
            mo = "IntegerArithmetics"
        
        commands.append(([
            "docker", "run", "--rm", "--init",
            "--volume", f"{BENCH_DIR}/contracts/{dataset}:/tmp:ro",
            "--entrypoint", "timeout",
            "mythril/myth:latest",
            "--kill-after=10", KILL_TIMEOUT,
            "/docker-entrypoint.sh",
            "analyze", f"/tmp/{id}.sol:{main_name}",
            "--execution-timeout", TIMEOUT,
            "--solv", v,
            "--transaction-count", "4", 
            "--modules", mo,
            "--outform", "json",
        ], id))
        
    return commands

def get_command_lent(df: pd.DataFrame, dataset: str):
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.12')
        if dataset == 'io':
            mo = "IntegerArithmetics"
        elif dataset == 'ls':
            mo = "EtherThief,AccidentallyKillable"
        elif dataset == 're':
            mo = "StateChangeAfterCall"
        elif dataset == 'pb':
            mo = "IntegerArithmetics"
            
        lent_timeout = "10800"
        if int(KILL_TIMEOUT) <= 20:
            lent_timeout = KILL_TIMEOUT
            
        commands.append(([
            "docker", "run", "--rm", "--init",
            "--volume", f"{BENCH_DIR}/contracts/{dataset}:/root/pgm:ro",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/tmp/output",
            "--entrypoint", "timeout",
            "my-lent:fse26",
            "--kill-after=10", lent_timeout,
            "/usr/local/bin/myth",
            "analyze", f"/root/pgm/{id}.sol:{main_name}",
            "--solv", v,
            "--modules", mo,
            "--use-lent9", "1",
            "--transaction-count", "4", # best setting of issta'24
            "--solver-timeout", "5000", # best setting of issta'24
            "--rlimit", "0",
            "--output-json", f"/tmp/output/{id}.json",
        ], id))
        
    return commands

def get_command_slither(df: pd.DataFrame, dataset: str):
    assert dataset != 'io'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.13')
        if dataset == 'io':
            exit(1)
        elif dataset == 'ls':
            mo = 'arbitrary-send-eth,suicidal'
        elif dataset == 're':
            mo = "reentrancy-eth"
            
            
        cmd = [
            "docker", "run", "--rm", "--init",
            "--volume", "smartrim-artifact-solc-select:/root/.solc-select:ro", 
            "--volume", f"{OUTPUT}/{id}:/root/output",
            "--mount", f"type=bind,src={BENCH_DIR}/contracts/{dataset}/{id}.sol,dst=/root/{id}.sol,ro",
            "--entrypoint", "timeout",
            "my-slither:fse26", 
            "--kill-after=10", KILL_TIMEOUT,
            "slither",
            f"/root/{id}.sol",
            '--solc-solcs-bin', f'/root/.solc-select/artifacts/solc-{v}/solc-{v}',
            '--detect', mo,
            '--json', f"/root/output/.json"
        ]
        
        commands.append((cmd, id))
        
    return commands

def get_command_achecker(df: pd.DataFrame, dataset: str):
    assert dataset == 'ls'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
            
        cmd = [
            "docker", "run", "--rm", "--init",
            "--volume", "smartrim-artifact-4smartian:/home/for-smartian:ro",
            "--entrypoint", "timeout",
            "--workdir", '/home/AChecker',
            "my-achecker:fse26",
            "--kill-after=10", KILL_TIMEOUT,
            "python3",
            "bin/achecker.py", 
            '--bytecode',
            '--file', f"/home/for-smartian/{dataset}.bin-runtime/{id}.bin_runtime",
            "--fib", '--sv'
        ]
        
        commands.append((cmd, id))
        
    return commands

def get_command_sailfish(df: pd.DataFrame, dataset: str):
    assert dataset == 're'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.11')
            
        cmd = [
            "docker", "run", "--rm", "--init",
            "--volume", f"{BENCH_DIR}/contracts/{dataset}:/root/pgm:ro",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/tmp/output",
            "--volume", f"smartrim-artifact-solc-select:/root/.solc-select:ro",
            "--entrypoint", "timeout",
            "holmessherlock/sailfish:latest",
            "--kill-after=10", KILL_TIMEOUT,
            "python",
            "contractlint.py",
            "-c", f"/root/pgm/{id}.sol",
            "-o", "/tmp/output",
            "-r", "range",
            "-p", "DAO",
            "-oo",
            "-sv", 'cvc4',
            "--solc-path", f"/root/.solc-select/artifacts/solc-{v}/solc-{v}",
        ]
        
        commands.append((cmd, id))
        
    return commands

def get_command_slise(df: pd.DataFrame, dataset: str):
    assert dataset == 're'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
            
        cmd = [
            "docker", "run", "--rm",
            "--volume", "smartrim-artifact-4smartian:/root/for-smartian:ro",   
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/output",
            "janson1060/slise_tool:v1",
            "bash", "-c", " ".join([
                f"source /SliSE_dev/py38/py38/bin/activate && ",
                f"cd /SliSE_dev/SliSE/SliSE && ",
                f"timeout --kill-after=10 {KILL_TIMEOUT} python SliSE.py /root/for-smartian/re.bin-runtime/{id}.bin_runtime -vt reentrancy && ",
                f"cp -r /SliSE_dev/SliSE/SliSE/detectiion_output/* /output",
            ])
        ]   
        
        commands.append((cmd, id))
        
    return commands

def get_command_efcf(df: pd.DataFrame, dataset: str):
    assert dataset != 'io'
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.11')
        
        if dataset == 'io':
            exit(1)
        elif dataset == 'ls':
            mo = ["--report-leaking-ether", "--report-dos-selfdestruct"]
        elif dataset == 're':
            mo = ["--report-leaking-ether"]
            
        cmd = [
            "docker", "run", "--rm", "--init",
            "--volume", f"{BENCH_DIR}/contracts/{dataset}:/root/pgm:ro",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/tmp/output",
            "--entrypoint", "timeout",
            "ghcr.io/uni-due-syssec/efcf-framework",
            "--kill-after=10", EFCF_KILL_TIMEOUT,
            "efcfuzz",
            "--out", "/tmp/output",
            "--source", f"/root/pgm/{id}.sol",
            "--solc-version", v,
            "--cores", "1",
            "--timeout", TIMEOUT,
            "--name", main_name,
        ]
        cmd.extend(mo)
        
        commands.append((cmd, id))
        
    return commands

def get_command_confuzzius(df: pd.DataFrame, dataset: str):
    commands = []
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        main_name = row['main_name']
        v = get_ver_from_metastr(row['compiler_version'])
        v = max_version(v, '0.4.11')
            
        cmd = [
            "docker", "run", "--rm",
            "--volume", f"{BENCH_DIR}/contracts/{dataset}:/root/pgm:ro",
            "--volume", f"{ARTIFACT_REPO}/output/{id}:/tmp/output",
            "--volume", "smartrim-artifact-solc-select:/root/.solc-select:ro", 
            "--env", f"ARTIFACT_SOLC_VERSION={v}",
            "--env", f"ARTIFACT_SOLC_DEST_LOCATION=/usr/local/lib/python3.6/dist-packages/solcx/bin/solc-v{v}",
            "--entrypoint", "timeout",
            "christoftorres/confuzzius",
            "--kill-after=10", CONFUZZIUS_KILL_TIMEOUT,
            "/root/.solc-select/solc-setter.sh",
            "python3", "fuzzer/main.py",
            "--evm", "byzantium",
            "--source", f"/root/pgm/{id}.sol",
            "--contract", main_name,
            "--solc", v,
            "--timeout", TIMEOUT,
            "--results", f"/tmp/output/{id}.json"
        ]
        
        commands.append((cmd, id))
        
    return commands

def get_command(df: pd.DataFrame, dataset: str):
    if program == 'rlf':
        return get_command_rlf(df, dataset)
    elif program == 'smartian':
        return get_command_smartian(df, dataset)
    elif program == 'mythril':
        return get_command_mythril(df, dataset)
    elif program == 'lent':
        return get_command_lent(df, dataset)
    elif program == 'smartest':
        return get_command_smartest(df, dataset)
    elif program == 'smartrim':
        return get_command_smartrim(df, dataset)
    elif program == 'smartrimbase':
        return get_command_smartrim(df, dataset, is_baseline=True)
    elif program == 'smartrimr':
        return get_command_smartrim(df, dataset, st='random')
    elif program == 'smartrimbaser':
        return get_command_smartrim(df, dataset, is_baseline=True, st='random')
    elif program == 'slither':
        return get_command_slither(df, dataset)
    elif program == 'achecker':
        return get_command_achecker(df, dataset)
    elif program == 'sailfish':
        return get_command_sailfish(df, dataset)
    elif program == 'slise':
        return get_command_slise(df, dataset)
    elif program == 'efcf':
        return get_command_efcf(df, dataset)
    elif program == 'confuzzius':
        return get_command_confuzzius(df, dataset)
    print('Unknown program name:', program)
    exit(1)

def main():
    parser = argparse.ArgumentParser()
    
    parser.add_argument('-t', '--tool', required=True, type=str, help="tool")
    parser.add_argument('-d', '--dataset', required=True, choices=['io', 'ls', 're', 'pb'], help="dataset")
    
    args, _ = parser.parse_known_args()
    
    with open(os.path.join(ARTIFACT_REPO, 'config.json')) as fp:
        config = json.load(fp)
    set_globals(args, config)
    
    os.system(f'cd {ARTIFACT_REPO}')
    print(os.getpid())
    
    if os.path.exists(OUTPUT):
        print("Please remove the output directory first (rm -fr ./output). Aborting")
        exit(88)
    output_replace = OUTPUT.replace('/output', f'/result/{args.dataset}/{program}')
    if os.path.exists(output_replace):
        print(f"Please remove the output directory first (rm -fr {output_replace}). Aborting")
        exit(88)
    
    os.mkdir(OUTPUT)
    
    base_dir = f"{BENCH_DIR}/contracts/{args.dataset}"
    metadata = f'{BENCH_DIR}/meta/{args.dataset}.csv'
    print(base_dir)
    
    df = pd.read_csv(metadata, dtype={'id': np.object_})
    df = df[~df['actual_order'].isna()]
    df = df.sort_values('actual_order')
    
    build_outdir(df['id'])
    
    commands = get_command(df, args.dataset)
    
    commands_with_count = []
    i = 0
    for cmd, id in commands:
        i += 1
        commands_with_count.append((cmd, id, i))
    
    n = len(commands_with_count)
    try:
        with Pool(processes=jobs) as pool:
            rets = [pool.apply_async(run_command, args=(cmd, id, i, n)) for cmd, id, i in commands_with_count]
            results = [res.get() for res in rets]
    
        pd.DataFrame({
            "file": [x[0] for x in results],
            "cmd": [x[1] for x in results],
            "start": [x[2] for x in results],
            "end": [x[3] for x in results],
            "retcode": [x[4] for x in results]
        }).to_csv(f'{OUTPUT}/cmd_history.csv', index=False)
            
        endtime_datetime = datetime.datetime.now()
        diff = endtime_datetime - starttime_datetime
        with open(f'{OUTPUT}/took.txt', 'w') as f:
            print(f'Time passed: {diff}', file=f)
        os.system(f'mv -f {OUTPUT} {output_replace}')
    finally:
        pass
    

if __name__ == '__main__':
    main()