import csv
import datetime
import json
import numpy as np
import os
import pandas as pd

PWD = os.getcwd()
HOME = os.path.expanduser('~')
ARTIFACT_REPO = PWD
BENCH_DIR = os.path.join(ARTIFACT_REPO, 'benchmark')
RESULT_DIR = os.path.join(PWD, 'result')

solv_lst = [
    "0.4.11", "0.4.12", "0.4.13", "0.4.14", "0.4.15", "0.4.16", "0.4.17", "0.4.18", "0.4.19", 
    "0.4.20", "0.4.21", "0.4.22", "0.4.23", "0.4.24", "0.4.25", "0.4.26",
    "0.5.0", "0.5.1", "0.5.2", "0.5.3", "0.5.4", "0.5.5", "0.5.6", "0.5.7", "0.5.8", "0.5.9", 
    "0.5.10", "0.5.11", "0.5.12", "0.5.13", "0.5.14", "0.5.15", "0.5.16", "0.5.17",
    "0.6.0", "0.6.1", "0.6.2", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.7", "0.6.8", "0.6.9", 
    "0.6.10", "0.6.11", "0.6.12",
    "0.7.0", "0.7.1", "0.7.2", "0.7.3", "0.7.4", "0.7.5", "0.7.6",
    "0.8.0", "0.8.1", "0.8.2", "0.8.3", "0.8.4", "0.8.5", "0.8.6", "0.8.7", "0.8.8", "0.8.9", 
    "0.8.10", "0.8.11", "0.8.12", "0.8.13", "0.8.14", "0.8.15", "0.8.16", "0.8.17", "0.8.18", "0.8.19", 
    "0.8.20", "0.8.21", "0.8.22", "0.8.23", "0.8.24", "0.8.25", "0.8.26", "0.8.27", "0.8.28", "0.8.29",
    "0.8.30",
]

def get_solv(row, minimum_required="0.4.11"):
    for solv in list(reversed(solv_lst)):
        # To avoid mismatch, iterate in reversed order (e.g., mismatch 0.6.11 to 0.6.1)
        if solv == minimum_required:
            return minimum_required
        if solv in row['compiler_version']:
            return solv
    return "0.4.25"

def get_solv_from_str(s: str, minimum_required="0.4.11"):
    # Mythril sometimes do not work
    for solv in list(reversed(solv_lst)):
        # To avoid mismatch, iterate in reversed order (e.g., mismatch 0.6.11 to 0.6.1)
        if solv == minimum_required:
            return minimum_required
        if solv in s:
            return solv
    return "0.4.25"

def get_main_name(meta_csv, fid):
    fp = open (meta_csv, 'r')
    rows = list(csv.DictReader(fp))
    rows = list(filter(lambda r: r['id'] == fid, rows))
    fp.close()
    assert len(rows) == 1

    row = rows[0]
    main_name = row['main_name']
    return main_name

def get_loc(meta_csv, fid):
    fp = open(meta_csv, 'r')
    rows = list(csv.DictReader(fp))
    rows = list(filter(lambda r: r['id'] == fid, rows))
    fp.close()
    assert len(rows) == 1

    row = rows[0]
    loc = row['loc']
    return loc

def sec_to_hm(sec):
    x = str(datetime.timedelta(seconds=sec)).split(':')
    assert(len(x) == 3)
    m = str(int(x[1]) + 1).zfill(2) if int(x[2]) >= 30 else x[1]
    hm = x[0] + 'h ' + m + 'm'
    return hm

def is_compilable(solv, full_fname):
    ret = os.system(f'solc_{solv} {full_fname} 2>/dev/null')
    return ret == 0

def json_load(filepath):
    try:
        fp = open(filepath, 'r')
        j = json.load(fp)
        fp.close()
        return (True, j)
    except:
        return (False, None)
    
def is_compilable_2(version: str, filepath: str):
    solc_path = solc_select_path(version)
    ret = os.system(f'{solc_path} {filepath} 2>/dev/null')
    return ret == 0

def solc_select_path(version: str, *, raise_if_not_installed=True):
    ret = f'{HOME}/.solc-select/artifacts/solc-{version}/solc-{version}'
    if raise_if_not_installed and not os.path.exists(ret):
        raise FileNotFoundError(ret)
    return ret

def meta(dataset):
    df = pd.read_csv(os.path.join(BENCH_DIR, 'meta', f'{dataset}.csv'), dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    return df

def ground(dataset):
    ids = meta(dataset)[['id']]
    df = pd.read_csv(os.path.join(BENCH_DIR, 'labels', f'{dataset}.csv'), dtype=np.object_)
    df = ids.merge(df, on='id', how='left')
    return df