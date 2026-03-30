import argparse
import numpy as np
import os
import pandas as pd
import subprocess
from utils import get_solv_from_str, solc_select_path
from collections import deque


HOME = os.path.expanduser('~')


def get_slithir(path: str, ver: str):
    # slither $IO/2018-12062.sol --print slithir
    solc_path = solc_select_path(ver)
    args = ['slither', path, '--print', 'slithir', '--solc', solc_path]
    ret = subprocess.run(args, capture_output=True)
    if ret.returncode != 0:
        raise ValueError("")
    out = ret.stderr.decode()
    out = out.split('INFO:Printers:')[1]
    out = out.split(f'INFO:Slither:{path}')[0].strip()
    return out


def _get_fn_name(s: str):
    return s.split(' ')[1].split('(')[0]


def _get_intcalls(src: str):
    ret: list[str] = []
    l = src.split('\n')
    for s in l:
        if 'INTERNAL_CALL' in s:
            ret.append(s.split(', ')[1].split('(')[0])
    return ret


def _contains_target(src: str) -> bool:
    targets = ['SEND', 'Transfer dest:', 'selfdestruct', 'suicide']
    for t in targets:
        if t in src:
            return True
    l = src.splitlines()
    for line in l:
        if 'HIGH_LEVEL_CALL' in line and 'value:' in line:
            return True
        if 'LOW_LEVEL_CALL' in line and 'value:' in line:
            return True
    return False


def _contains_target_su_only(src: str) -> bool:
    targets = ['selfdestruct', 'suicide']
    for t in targets:
        if t in src:
            return True
    return False


def _search(intcalls_dict: dict[str, list[str]], targets_dict: dict[str, bool]):
    assert list(intcalls_dict.keys()) == list(targets_dict.keys())
    reverse_dict: dict[str, list[str]] = {x: [] for x in intcalls_dict.keys()}
    for fn in intcalls_dict:
        nexts = intcalls_dict[fn]
        for next in nexts:
            reverse_dict[next].append(fn)
    
    visited = {x: False for x in targets_dict.keys()}
    q = deque()
    for fn in targets_dict:
        if targets_dict[fn] == False:
            continue
        visited[fn] = True
        q.append(fn)
        
    while len(q) > 0:
        fn: str = q.popleft()
        nexts = reverse_dict[fn]
        for next in nexts:
            if visited[next] == True:
                continue
            visited[next] = True
            q.append(next)
    
    return [fn for fn in visited.keys() if visited[fn] == True]


def _get_target(s: str, su_only=False):
    l = s.splitlines()
    contract_indices = [i for i in range(len(l)) if l[i][0:8] == 'Contract']
    contract_indices.append(len(l))
    contract_splitted = [
        '\n'.join(l[contract_indices[i]+1:contract_indices[i+1]]) 
        for i in range(len(contract_indices) - 1)
    ]
    contract_erased = '\n'.join(contract_splitted)
    l = contract_erased.splitlines()
    function_indices = [i for i in range(len(l)) if l[i][0:9] in ['\tFunction', '\tModifier']]
    function_indices.append(len(l))
    function_splitted = [
        (l[function_indices[i]], '\n'.join(l[function_indices[i]+1:function_indices[i+1]]))
        for i in range(len(function_indices) - 1)
    ]
    function_splitted = [(_get_fn_name(a), b) for (a, b) in function_splitted]
    
    intcalls_dict = {fn: _get_intcalls(src) for (fn, src) in function_splitted}
    if su_only == False:
        targets_dict = {fn: _contains_target(src) for (fn, src) in function_splitted}
    else:
        targets_dict = {fn: _contains_target_su_only(src) for (fn, src) in function_splitted}
    ret = _search(intcalls_dict, targets_dict)
    return ret
    
    
def run(path: str, ver: str, su_only=False) -> list[str]:
    slithir = get_slithir(path, ver)
    targets = _get_target(slithir, su_only=su_only)
    targets = [fn.split('.')[1] for fn in targets]
    targets = ['@fallback' if fn == 'fallback' else fn for fn in targets]
    return targets
    

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--path', type=str)
    parser.add_argument('--ver', type=str)
    args = parser.parse_args()
    print(run(args.path, args.ver))
    
    
def _validate(su_only=False):
    df = pd.read_csv(f'{HOME}/smartrim-benchmark/meta/ls.csv', dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    ground = pd.read_csv(f'{HOME}/smartrim-benchmark/labels/ls.csv', dtype=np.object_).fillna('')
    df = df.merge(ground, on='id', how='left').fillna('')
    df['candidates'] = ''
    
    for i in df.index:
        id = df.loc[i]['id']
        if id == '0x2910536d9d858760c440d89f7a3821a77708933c':
            continue # slithIR internal error
        if id == '0x9e6c849ab00670ece0ddcb1b16afadfba7c21eb9':
            continue
        if id == '0xd87d3d9fb80a5f009b3812b536256b6f457176b1':
            continue
        v = get_solv_from_str(df.loc[i]['compiler_version'], '0.4.16')
        if v == '0.5.0':
            v = '0.5.1'
        path = f'{HOME}/smartrim-benchmark/contracts/ls/{id}.sol'
        candids = run(path, v, su_only=su_only)
        if su_only == False:
            grounds = set(df.loc[i]['el-f'].replace(':', '/').split('/'))
            grounds = grounds.union(set(df.loc[i]['su-f'].replace(':', '/').split('/')))
        else:
            grounds = set(df.loc[i]['su-f'].replace(':', '/').split('/'))
        if '' in grounds:
            grounds.remove('')
        for ground in grounds:
            if ground not in candids:
                print(ground, candids)
                assert False
        print(id)
        df.loc[i, 'candidates'] = '/'.join(candids) if len(candids) > 0 else '-'
    if su_only == False:
        df[['id', 'candidates']].to_csv('assets/candidates-el.csv', index=False)
    else:
        df[['id', 'candidates']].to_csv('assets/candidates-su.csv', index=False)
    

if __name__ == '__main__':
    _validate(su_only=False)
    _validate(su_only=True)
