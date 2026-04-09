"""
parsing results
"""

import json
import matplotlib.pyplot as plt
from matplotlib.ticker import MultipleLocator
import numpy as np
import os
import pandas as pd
import sys

from analyze_tool import LS_TOOL, IO_TOOL, RE_TOOL
from utils import BENCH_DIR, RESULT_DIR

HOME = os.path.expanduser('~')

def _meta(dataset):
    df = pd.read_csv(f"{BENCH_DIR}/meta/{dataset}.csv", dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    return df
 
def _ground(dataset):
    ids = _meta(dataset)[['id']]
    df = pd.read_csv(f"{BENCH_DIR}/labels/{dataset}.csv", dtype=np.object_)
    df = ids.merge(df, on='id', how='left')
    return df

def ground_as_buglist(dataset, kind: str):
    df = _ground(dataset)
    ret = []
    for i in df.index:
        s = df.loc[i][f'{kind}']
        if str(s) in ["", "-", "nan"]:
            continue
        s = s.split('/')
        for x in s:
            ret.append((df.loc[i]['id'], x))
    return ret
    
def ground_num(dataset: str, kind: str):
    return len(ground_as_buglist(dataset, kind))
    
def _mk_smartrim_buginfo(dataset, kind: str, is_base=False, strategy='inc'):
    df = _meta(dataset)
    ret = pd.DataFrame({
        "id": [],
        "line": [],
        "offset": [],
        "len": [],
        "time": [],
        "depth": [],
        "validated": [],
        "tseq": []
    })
    base_suffix = "base" if is_base else ""
    if strategy == 'inc':
        strategy_suffix = ''
    elif strategy == 'r':
        strategy_suffix = 'r'
        
    for i in df.index:
        id = df.loc[i]['id']
        dir = f"{RESULT_DIR}/{dataset}/smartrim{base_suffix}{strategy_suffix}"
        if not os.path.exists(f"{dir}/{id}/{id}/{id}.json"):
            continue
        with open(f"{dir}/{id}/{id}/{id}.json", "r") as fp:
            j = json.load(fp)
        if j['result'] is None:
            continue
        r = j['result']
        for bug in r:
            info = bug['tseq_info']
            if bug['kind'][0].upper()[:2] != kind.upper()[:2]:
                continue
            time = info['disproven_time']
            line = info['line']
            offset = info['loc']['offset']
            len_ = info['loc']['len']
            depth = info['depth']
            validated = info['validate_code']
            tseq = info['tseq_string']
            ret.loc[len(ret)] = {
                'id': id, 'line': line, 'offset': offset, 'len': len_, 'time': time, 
                'depth': depth, 'validated': validated, 'tseq': tseq
            }
    ret['kind'] = kind
    base_suffix2 = '.base' if is_base else ""
    strategy_suffix2 = '' if strategy_suffix == '' else '.' + strategy_suffix
    filename = f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}{base_suffix2}{strategy_suffix2}.csv'
    dirname = os.path.dirname(filename)
    os.makedirs(dirname, exist_ok=True)
    ret.to_csv(
        f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}{base_suffix2}{strategy_suffix2}.csv', 
        index=False
    )
    
def mk_smartrim_buginfo():
    _mk_smartrim_buginfo('io', 'io')
    _mk_smartrim_buginfo('io', 'io', is_base=True)
    _mk_smartrim_buginfo('ls', 'el')
    _mk_smartrim_buginfo('ls', 'el', is_base=True)
    _mk_smartrim_buginfo('ls', 'su')
    _mk_smartrim_buginfo('ls', 'su', is_base=True)
    _mk_smartrim_buginfo('re', 're')
    _mk_smartrim_buginfo('re', 're', is_base=True)
    _mk_smartrim_buginfo('io', 'io', strategy='r')
    _mk_smartrim_buginfo('io', 'io', is_base=True, strategy='r')
    _mk_smartrim_buginfo('ls', 'el', strategy='r')
    _mk_smartrim_buginfo('ls', 'el', is_base=True, strategy='r')
    _mk_smartrim_buginfo('ls', 'su', strategy='r')
    _mk_smartrim_buginfo('ls', 'su', is_base=True, strategy='r')
    _mk_smartrim_buginfo('re', 're', strategy='r')
    _mk_smartrim_buginfo('re', 're', is_base=True, strategy='r')
    
def _get_smartrim_fp(dataset: str, kind: str):
    l = ground_as_buglist(dataset, kind)
    s = set(l)
    df_found = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}.csv', dtype=np.object_)
    ret = []
    for i in df_found.index:
        id, line = df_found.loc[i]['id'], df_found.loc[i]['line']
        validated = df_found.loc[i]['validated']
        if (id, line) not in s:
            if dataset == 'io' and validated == "0":
                continue
            ret.append((id, line))
    print(len(ret))
    for id, line in ret:
        print(id, line)
    
def get_smartrim_fp():
    mk_smartrim_buginfo()
    _get_smartrim_fp('io', 'io')
    _get_smartrim_fp('ls', 'el')
    _get_smartrim_fp('ls', 'su')
    _get_smartrim_fp('re', 're')
    
def _get_smartrim_fn(dataset: str, kind: str):
    l = ground_as_buglist(dataset, kind)
    df_found = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}.csv', dtype=np.object_)
    s2: set[tuple[int, int]] = set()
    ret = []
    for i in df_found.index:
        id, line = df_found.loc[i]['id'], df_found.loc[i]['line']
        s2.add((id, line))
    for ground_id, ground_loc in l:
        if (ground_id, ground_loc) not in s2:
            ret.append((ground_id, ground_loc))
    
    print(len(ret))
    for id, loc in ret:
        print(id, loc)
    
def get_smartrim_fn():
    _get_smartrim_fn('io', 'io')
    _get_smartrim_fn('ls', 'el')
    _get_smartrim_fn('ls', 'su')
    _get_smartrim_fn('re', 're')
    
def _count_validated(dataset: str, kind: str):
    df_found = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}.csv', dtype=np.object_)
    return len(df_found), len(df_found[df_found['validated'] == '0'])

def count_validated():
    iof, iov = _count_validated('io', 'io')
    elf, elv = _count_validated('ls', 'el')
    suf, suv = _count_validated('ls', 'su')
    ref, rev = _count_validated('re', 're')
    totf = iof + elf + suf + ref
    totv = iov + elv + suv + rev
    print(totf, totv, totv / totf)
    
def _mk_statistics_unit(depth, dataset, is_base=False):
    df = _meta(dataset)
    ret = pd.DataFrame({
        "id": [],
        "iter": [],
        "v": [],
        "w": [],
        "time": [],
    })
    base_suffix = "base" if is_base else ""
    for i in df.index:
        id = df.loc[i]['id']
        dir = f"{RESULT_DIR}/{dataset}/smartrim{base_suffix}"
        if not os.path.exists(os.path.join(dir, id, id, id + '.json')):
            continue
        with open(os.path.join(dir, id, id, id + '.json'), "r") as fp:
            j = json.load(fp)
        if j['summary'] is None or 'iter_each_depth' not in j['summary']:
            continue
        summ = j['summary']
        if len(summ['iter_each_depth']) <= depth:
            continue
        iter = summ['iter_each_depth'][depth]
        v = summ['v_each_depth'][depth]
        w = summ['w_each_depth'][depth]
        w = 1 if w == 0 else w
        time = float(summ['time_each_depth'][depth])
        ret.loc[len(ret)] = {'id': id, 'iter': iter, 'v': v, 'w': w, 'time': time}
    ret['dataset'] = dataset
    return ret


def mk_statistics(depth: int):
    if type(depth) == str:
        depth = int(depth)
    df_iob = _mk_statistics_unit(depth, 'io', is_base=True)
    df_iop = _mk_statistics_unit(depth, 'io', is_base=False)
    df_lsb = _mk_statistics_unit(depth, 'ls', is_base=True)
    df_lsp = _mk_statistics_unit(depth, 'ls', is_base=False)
    df_reb = _mk_statistics_unit(depth, 're', is_base=True)
    df_rep = _mk_statistics_unit(depth, 're', is_base=False)
    df_io = df_iob.merge(df_iop, how='outer', on='id', suffixes=['b', 'p'])
    df_ls = df_lsb.merge(df_lsp, how='outer', on='id', suffixes=['b', 'p'])
    df_re = df_reb.merge(df_rep, how='outer', on='id', suffixes=['b', 'p'])
    df = pd.concat([df_io, df_ls, df_re])
    df = df[~df['datasetb'].isna()]
    df = df[~df['datasetp'].isna()]
    df = df.reset_index(drop=True)
    print("DF:", len(df))
    print('times:', sum(df['timep']) / sum(df['timeb']))
    print('iters:', sum(df['iterp']) / sum(df['iterb']))
    print('v:', sum(df['vp']) / sum(df['vb']))
    print('v2:', sum(df['vp'] / df['vb']) / len(df))
    print('w:', sum(df['wp']) / sum(df['wb']))
    print('w2:', sum(df['wp'] / df['wb']) / len(df))
    
def _get_pruning_overhead(dataset):
    df = _meta(dataset)
    ret = []
    for i in df.index:
        id = df.loc[i]['id']
        dir = f"{RESULT_DIR}/{dataset}/smartrim"
        dir = f"{RESULT_DIR}/{dataset}/smartrim"
        if not os.path.exists(f"{dir}/{id}/{id}/{id}.json"):
            continue
        with open(f"{dir}/{id}/{id}/{id}.json", "r") as fp:
            j = json.load(fp)
        if j['summary'] is None or 'iter_each_depth' not in j['summary']:
            continue
        summ = j['summary']
        time = float(j['cpu_time'])
        overhead = summ["subs_total_overhead"]
        ret.append(overhead / time)
        
    return ret


def get_pruning_overhead():
    ret = []
    ret.extend(_get_pruning_overhead('io'))
    ret.extend(_get_pruning_overhead('ls'))
    ret.extend(_get_pruning_overhead('re'))
    print(sum(ret) / len(ret))

    
def ground_truth_info(dataset: str, kind: str):
    df = _ground(dataset)
    dfmeta = _meta(dataset)
    print(len(dfmeta))
    ids = dfmeta[['id']]
    gt = ids.merge(df, how='left', on='id')
    func = 0
    line = 0
    for s in gt[f'{kind}']:
        if str(s) in ['', '-', 'nan']:
            continue
        #print(s, s.count('/') + 1)
        line += s.count('/') + 1
    for s in gt[f'{kind}-f']:
        if str(s) in ['', '-', 'nan']:
            continue
        func += s.count('/') + 1
    return func, line

def count_pruning_regression(dataset: str, kind: str):
    df1 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}.csv')
    df2 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dataset}.{kind}.base.csv')
    df = df1.merge(df2, on=['kind', 'id', 'line', 'offset', 'len'], how='outer', suffixes=['p', 'b'])
    df = df[df['timep'].isna()]
    print(len(df))
    print(df[['id', 'line', 'depthb', 'validatedb', 'tseqb']])
    
def get_all_dataset_time(time=1800, depth=4, strategy='r', gt_filter=True):
    mk_smartrim_buginfo()
    if type(time) == str:
        time = int(time)
    if type(depth) == str:
        depth = int(depth)
    btimes = []
    ptimes = []
    for dk in ['io.io', 'ls.el', 'ls.su', 're.re']:
        ids = _meta(dk.split('.')[0])[['id']]
        dfb = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dk}.base.csv')
        dfp = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dk}.csv')
        dflb = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dk}.base.r.csv')
        dflp = pd.read_csv(f'{RESULT_DIR}/summary/ablation/{dk}.r.csv')
        # dfb = ids.merge(dfb, on='id', how='left')
        # dfp = ids.merge(dfp, on='id', how='left')
        # dflb = ids.merge(dflb, on='id', how='left')
        # dflp = ids.merge(dflp, on='id', how='left')
        on = ['kind', 'id', 'line', 'offset', 'len']
        dfb = dfb.rename(columns=lambda x: x + 'b' if x not in on else x)
        dfp = dfp.rename(columns=lambda x: x + 'p' if x not in on else x)
        dflb = dflb.rename(columns=lambda x: x + 'rb' if x not in on else x)
        dflp = dflp.rename(columns=lambda x: x + 'rp' if x not in on else x)
        df = dfb.merge(dfp, on=on, how='outer')
        df = df.merge(dflb, on=on, how='outer')
        df = df.merge(dflp, on=on, how='outer')
        for i in range(depth):
            df = df[df['depthb'] != i]
        for i in range(depth):
            df = df[df['depthp'] != i]
        for i in range(depth):
            df = df[df['depthrb'] != i]
        for i in range(depth):
            df = df[df['depthrp'] != i]
        #df.to_csv(f'{RESULT_REPO}/summary/{dk}-depth4.csv', index=False)
        if gt_filter == True:
            length_gt = pd.read_csv(os.path.join(BENCH_DIR, 'labels', 'l4.csv'))
            df = length_gt.merge(df, on=on, how='left')
            df = df[df['included'] == 'o']
            
        if strategy == 'r':
            btimes.extend(df['timerb'])
            ptimes.extend(df['timerp'])
        else:
            btimes.extend(df['timeb'])
            ptimes.extend(df['timep'])
    btimes = [x for x in btimes if str(x) != 'nan']
    ptimes = [x for x in ptimes if str(x) != 'nan']
    btimes_cut = [x for x in btimes if x < time]
    ptimes_cut = [x for x in ptimes if x < time]
    btimes_cut.sort()
    ptimes_cut.sort()
    print(f"B: {len(btimes_cut)}")
    print(f"P: {len(ptimes_cut)}")
    return btimes_cut, ptimes_cut
    
    
def _draw_cac(strategy='inc'):
    depth = 4
    btimes, ptimes = get_all_dataset_time(depth=depth, strategy=strategy)
    nb = len(btimes)
    np = len(ptimes)
    
    plt.figure(figsize=(8, 6))

    strategy = 'Inc' if strategy == 'inc' else 'Random' if strategy == 'r' else ''
    plt.plot(btimes, list(range(nb)), marker='x', label=strategy)
    plt.plot(ptimes, list(range(np)), marker='o', label=f'{strategy} + Pruning')

    plt.xlabel('Time budget (s) per contract', fontsize=22)
    plt.ylabel(f'#bugs (length ≥ {depth})', fontsize=22)
    plt.tick_params(axis='both', which='major', labelsize=15)
    plt.legend(fontsize=22, loc='lower right')
    plt.grid(True)
    plt.tight_layout()
    plt.xlim(0, 1800)
    plt.gca().xaxis.set_major_locator(MultipleLocator(250))
    plt.savefig(f'{RESULT_DIR}/{strategy}-d{depth}.pdf')
    
    
def draw_cac():
    _draw_cac(strategy='inc')

def draw_cac_r():
    _draw_cac(strategy='r')
    
    
def count_abl(dataset: str, kind: str, time: int):
    if type(time) == str:
        time = int(time)
    gt = ground_as_buglist(dataset, kind)
    gt = set(gt)
    df1 = pd.read_csv(f'result/bugs.{dataset}.{kind}..csv')
    df2 = pd.read_csv(f'result/bugs.{dataset}.{kind}.base.csv')
    df = df1.merge(df2, on=['kind', 'id', 'line', 'offset', 'len'], how='outer', suffixes=['p', 'b'])
    dfb = df[~df['timeb'].isna()]
    dfp = df[~df['timep'].isna()]
    dfb = dfb[dfb['timeb'] < time]
    dfp = dfp[dfp['timep'] < time]
    if dataset == 'cve':
        print(len(dfb), len(dfp))
        return
    cntb = 0
    cntp = 0
    for i in dfb.index:
        id = dfb.loc[i]['id']
        line = dfb.loc[i]['line']
        line = str(line)
        if (id, line) in gt:
            cntb += 1
    for i in dfp.index:
        id = dfp.loc[i]['id']
        line = dfp.loc[i]['line']
        line = str(line)
        if (id, line) in gt:
            cntp += 1
    print(cntb, cntp)
    
def count_abl2():
    df1 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/io.io.csv', dtype=np.object_)
    df2 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/ls.el.csv', dtype=np.object_)
    df3 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/ls.su.csv', dtype=np.object_)
    df4 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/re.re.csv', dtype=np.object_)
    df_p = pd.concat([df1, df2, df3, df4])
    print(len(df_p))
    df1 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/io.io.base.csv', dtype=np.object_)
    df2 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/ls.el.base.csv', dtype=np.object_)
    df3 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/ls.su.base.csv', dtype=np.object_)
    df4 = pd.read_csv(f'{RESULT_DIR}/summary/ablation/re.re.base.csv', dtype=np.object_)
    df_b = pd.concat([df1, df2, df3, df4])
    print(len(df_b))
    df_all = df_b.merge(df_p, how='inner', on=['kind', 'id', 'line', 'offset', 'len'], suffixes=['b', 'p'])
    #df_all = df_all[df_all['depthb'].astype(int) >= 4]
    #df_all = df_all[df_all['depthp'].astype(int) >= 4]
    print(f'all: {len(df_all)}')
    print(f'base<prun: {len(df_all[df_all["timeb"].astype(float) < df_all["timep"].astype(float)])}')
    print(sum(df_all['timeb'].astype(float)))
    print(sum(df_all['timep'].astype(float)))
    
def find_smartrim_only(dataset: str):
    if dataset == 'ls':
        tools = LS_TOOL
    elif dataset == 'io':
        tools = IO_TOOL
    elif dataset == 're':
        tools = RE_TOOL
    else:
        assert False
        
    df = pd.read_csv(os.path.join(RESULT_DIR, 'summary', dataset, 'bugs', f'smartrim.csv'), dtype=np.object_)
    
    for tool in tools:
        if tool.startswith('smartrim'):
            continue
        
        df1 = pd.read_csv(os.path.join(RESULT_DIR, 'summary', dataset, 'bugs', f'{tool}.csv'), dtype=np.object_)
        df1['label'] = True
        if 'line' in df1.keys():
            my_on = 'line'
        else:
            my_on = 'func'
            
        df = df.merge(df1, on=['id', 'kind', my_on], how='outer', suffixes=[None, 'X'])
        df = df[df['label'].isna()]
        df = df[['id', 'kind', 'func', 'line']]
        
    df.to_csv(os.path.join(RESULT_DIR, 'summary', f'{dataset}-only-smartrim.csv'), index=False)
    return df
    
def help():
    print([x for x in globals().keys() if not x.startswith('_')])

if __name__ == '__main__':
    argv = sys.argv[:]
    if len(argv) > 1:
        func_name = argv[1]
        if func_name.startswith('_'):
            print('Do not enter private functions. abort')
            exit(1)
        if func_name in globals():
            ret = globals()[func_name](*argv[2:])
            if ret is not None:
                print(ret)
        else:
            print(f"Error: Function '{func_name}' not found.")
            exit(1)
    else:
        mk_smartrim_buginfo()
        draw_cac()
        draw_cac_r()
