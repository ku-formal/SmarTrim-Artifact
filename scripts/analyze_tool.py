import argparse
import json
import math
import numpy as np
import os
import pandas as pd
import re
import sys

from utils import BENCH_DIR, RESULT_DIR, get_solv_from_str


LS_TOOL = ['smartrim', 'smartest', 'efcf', 'smartian', 'confuzzius', 'rlf', 'mythril', 'lent', 'slither', 'achecker']
IO_TOOL = ['smartrim', 'smartest', 'smartian', 'confuzzius', 'mythril', 'lent']
RE_TOOL = ['smartrim', 'efcf', 'smartian', 'confuzzius', 'mythril', 'lent', 'slither', 'sailfish', 'slise']
TOOL = sorted(set(LS_TOOL) | set(IO_TOOL) | set(RE_TOOL))


def find_func(selector: str, dataset: str):
    selector = selector[2:] if selector.startswith('0x') else selector
    selector_df = pd.read_csv(f"{BENCH_DIR}/misc/selectors/{dataset}.csv", dtype=np.object_)
    selectors = selector_df[selector_df['selector'] == selector]
    return list(selectors['function'])[0]


def df_format_number(x):
    if x == 0:
        return ""
    elif math.isnan(x):
        return ""
    elif int(x) == x:
        return str(int(x))
    else:
        return str(x)


def get_bug_types(dataset):
    if dataset == 'io':
        return ['io']
    elif dataset == 'ls':
        return ['el', 'su']
    elif dataset == 're':
        return ['re']
    else:
        assert False
        
        
def normalize_fallback(s, tool):
    if tool == 'smartian':
        if s == 'fallback':
            return '@fallback'
    return s


def extract_findings(df, tool: str, dataset: str, output_path: str = RESULT_DIR) -> pd.DataFrame:
    """
    :param output_path: where is the output of tools
    """
    bug_types = get_bug_types(dataset)
    output_path = os.path.join(RESULT_DIR, dataset, tool)
    
    if tool == 'smartrim':
        df_ret, df_bug = extract_from_smartrim(df, output_path, bug_types)
    elif tool == 'smartest':
        df_ret, df_bug = extract_from_smartest(df, output_path, bug_types)
    elif tool == 'mythril' or tool == 'lent':
        df_ret, df_bug = extract_from_myth_or_lent(df, output_path, bug_types, tool)
    elif tool == 'rlf':
        df_ret, df_bug = extract_from_rlf(df, dataset, output_path, bug_types)
    elif tool == 'smartian':
        df_ret, df_bug = extract_from_smartian(df, output_path, bug_types)
    elif tool == 'achecker':
        assert dataset == 'ls'
        df_ret, df_bug = extract_from_achecker(df, output_path)
    elif tool == 'slither':
        df_ret, df_bug = extract_from_slither(df, output_path, bug_types)
    elif tool == 'sailfish':
        df_ret, df_bug = extract_from_sailfish(df, dataset, output_path)
    elif tool == 'slise':
        df_ret, df_bug = extract_from_slise(df, dataset, output_path)
    elif tool == 'confuzzius':
        df_ret, df_bug = extract_from_confuzzius(df, dataset, output_path, bug_types)
    elif tool == 'efcf':
        df_ret, df_bug = extract_from_efcf(df, output_path, bug_types)
    else:
        raise ValueError(f"unknown tool name: {tool}")
        
    bugdir = os.path.join(RESULT_DIR, 'summary', dataset, 'bugs')
    os.makedirs(bugdir, exist_ok=True)
    
    df_bug.to_csv(os.path.join(RESULT_DIR, 'summary', dataset, 'bugs', f'{tool}.csv'), index=False)
    df_ret = postproc(df_ret, df_bug, bug_types)
    return df_ret

def extract_from_smartrim(df_meta: pd.DataFrame, output_path, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
        'line': [],
    })
    for i in df_meta.index:
        id = df_meta.loc[i]['id']
        json_path = os.path.join(output_path, id, id, f'{id}.json')
        if not os.path.exists(json_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        with open(json_path, "r") as fp:
            j = json.load(fp)
        if j['result'] is None:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        r = j['result']
        df.loc[i] = {'id': id, 'succeed': True}
        for bug in r:
            # if bug['tseq_info']['validate_code'] != 0: pass
            kind = bug['kind'][0].lower()
            if kind == 're_el':
                kind = 're'
            if kind not in bug_types:
                continue
            line = bug['tseq_info']['line']
            line = str(line)
            func = bug['tseq'][-1]['fname']
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': func, 'line': line}
    assert len(df) == len(df_meta)
    return df, df_bug
    
    
def _parse_smartest_unit(s: str):
    assert isinstance(s, str)
    ret: list[tuple[str, str, str]] = []
    
    def starts_with_integer(s: str):
        s = s.lstrip()
        return s[0].isdecimal()

    s = s.split('=== Report ===')[1].split('=== Statistics ===')[0]
    l = s.split('\n\n')
    l = [s.strip() for s in l]
    l = [s for s in l if 'disproven' in s]
    
    for s in l:
        lines = s.split('\n')
        line_line = lines[0]
        func_line = lines[1:]
        
        kind = line_line.split(' ')[1]
        kind = (
            'io' if kind == '[IO]' else
            'el' if kind == '[ETH_LEAK]' else
            'su' if kind == '[KA]' else
            ''
        )
        line = line_line.split(' ')[3][:-1]
        _ = int(line) # assert that it is an integer
        
        func_name = [x for x in func_line if starts_with_integer(x)][-1]
        func_name = func_name.lstrip()[3:]
        func_name = func_name.replace('smartest_', '')
        ret.append((kind, func_name, line))
    return ret

def extract_from_smartest(df_meta: pd.DataFrame, output_path, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
        'line': [],
    })
    for i in df_meta.index:
        id = df_meta.loc[i]['id']
        json_path = os.path.join(output_path, id, f'{id}.json')
        stdout_path = os.path.join(output_path, id, '.stdout.txt')
        if not os.path.exists(json_path) or os.path.getsize(json_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        if not os.path.exists(stdout_path) or os.path.getsize(stdout_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        with open(json_path, "r") as fp:
            j = json.load(fp)
        with open(stdout_path, "r") as fp:
            out = fp.read()
        if j['result'] is None:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        if '=== Report ===' not in out:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        df.loc[i] = {'id': id, 'succeed': True}
        ret = _parse_smartest_unit(out)
        for kind, func, line in ret:
            if kind not in bug_types:
                continue
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': func, 'line': line}
    assert len(df) == len(df_meta)
    return df, df_bug

def extract_from_myth_or_lent(df_meta: pd.DataFrame, output_path, bug_types, tool):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
        'line': [],
    })

    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        if tool == 'mythril':
            log_path = os.path.join(dir_path, '.stdout.txt')
        elif tool == 'lent':
            log_path = os.path.join(dir_path, f"{id}.json")
            
        # errmsg_path = os.path.join(dir_path, '.stderr.txt')
        # it seems that there is nothing to learn in the error logs if the tool is `mythril`

        if not os.path.isfile(log_path) or os.path.getsize(log_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        try:
            with open(log_path, 'r', encoding='utf-8') as f:
                s = f.read()
                if s.startswith('timeout: the monitored command dumped core'):
                    raise ValueError(s)
                data = json.loads(s)
        except ValueError:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        except Exception:
            print(id)
            assert False
            
        df.loc[i] = {'id': id, 'succeed': True}

        for issue in data['issues']:
            title = issue['title']
            kind = (
                'el' if "ether" in title or "Ether" in title else
                'su' if "selfdestruct" in title or "suicide" in title or "Selfdestruct" in title else
                'io' if "overflow" in title or "underflow" in title or "Integer Arithmetic Bugs" in title else
                're' if "external call" in title or "state" in title else
                'unknown-key'
            )
            if kind not in bug_types:
                print(id, kind, issue['title'])
                assert False
            func = issue['function']
            line = issue['lineno'] if 'lineno' in issue else 0
            if func == 'fallback':
                func = '@fallback'
            else:
                func = func.split('(')[0]
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': func, 'line': str(line)}
    assert len(df) == len(df_meta)
    return df, df_bug

def extract_from_rlf(df_meta: pd.DataFrame, dataset: str, result_path: str, bug_types):
    assert dataset == 'ls'
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })
    
    for i in df_meta.index:
        id = str(df_meta['id'][i])
        json_path = os.path.join(result_path, id, 'None.json')
        if not os.path.isfile(json_path) or os.path.getsize(json_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        with open(json_path, "r") as fp:
            j = json.load(fp)
            
        df.loc[i] = {'id': id, 'succeed': True}
        
        j = j['bug_finder']
        leak = j["Leaking"] if "Leaking" in j else {}
        su = j["Suicidal"] if "Suicidal" in j else {}
        for fn in leak:
            fn_ = fn if fn != 'default' else '@fallback'
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'el', 'func': fn_}
        for fn in su:
            fn_ = fn if fn != 'default' else '@fallback'
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'su', 'func': fn_}
    assert len(df) == len(df_meta)
    return df, df_bug
        

def extract_from_smartian(df_meta: pd.DataFrame, result_path: str, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })

    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(result_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        log_path = os.path.join(dir_path, '.stdout.txt')
        errmsg_path = os.path.join(dir_path, '.stderr.txt')
        bug_dir = os.path.join(dir_path, 'bug')

        if not os.path.isfile(log_path) or os.path.getsize(log_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        with open(log_path, 'r') as f:
            log_lines = f.readlines()
        with open(errmsg_path, 'r') as f:
            errmsg_lines = f.readlines()

        if any(line.startswith('Unhandled exception. System') for line in errmsg_lines):
            df.loc[i] = {'id': id, 'succeed': False,}
            continue
        if any(line.startswith('Stack overflow.') for line in errmsg_lines):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        if not os.path.isdir(bug_dir):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        df.loc[i] = {'id': id, 'succeed': True}
        for bugfile in os.listdir(bug_dir):
            bugid = os.path.splitext(bugfile)[0]
            try:
                with open(os.path.join(bug_dir, bugfile), 'r') as bf:
                    jdata = json.load(bf)
            except Exception:
                assert False

            idx = next((i for i, s in enumerate(log_lines) if f'Save bug seed {bugid}' in s), None)
            if idx is None or idx - 1 < 0 or 'Tx#' not in log_lines[idx - 1]:
                assert False

            try:
                bug_tx = int(log_lines[idx - 1].split('Tx#')[1].split(' found ')[0])
            except Exception:
                assert False

            txs = jdata.get('Txs', [])
            if not (0 <= bug_tx - 1 < len(txs)):
                assert False

            func = txs[bug_tx - 1].get('Function', '').split('(')[0]
            func = normalize_fallback(func, 'smartian')
            key = (
                'io' if 'IB' in bugid else
                'el' if 'EL' in bugid else
                'su' if 'SC' in bugid else
                're' if 'RE' in bugid else ''
            )
            if key in bug_types:
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': key, 'func': func}
                
    assert len(df) == len(df_meta)
    return df, df_bug
    
def is_selector(name):
    return bool(re.fullmatch(r'[0-9a-fA-F]{8}', name)) or bool(re.fullmatch(r'0x[0-9a-fA-F]{8}', name))

def clean_func_name(dataset, name):
    name = name.strip()
    if name in ('()', ''):
        return '@fallback'
    if is_selector(name):
        return find_func(name, dataset)
    return re.sub(r'\(.*', '', name)

def extract_from_achecker(df_meta: pd.DataFrame, result_path: str):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })

    pattern_vac = re.compile(r'Violated access control check in function ([A-Za-z0-9_(),]+)')
    pattern_np = re.compile(r'Non protected SELFDESTRUCT.*in function ([A-Za-z0-9_(),]+)')
    pattern_ca = re.compile(r'Controlable Address of SELFDESTRUCT.*in function ([A-Za-z0-9_(),]+)')

    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(result_path, id)
        log_path = os.path.join(dir_path, '.stdout.txt')

        if not os.path.isfile(log_path) or os.path.getsize(log_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        success = True
        with open(log_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
            if not lines or not lines[0].startswith("Checking contract for "):
                success = False
                break
        if not success:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        df.loc[i] = {'id': id, 'succeed': True}

        np_funcs = [] # su
        ca_funcs = [] # el
        vac_funcs = [] # potential el,su
        for line in lines:
            m_np = pattern_np.search(line)
            if m_np:
                np_funcs.append(m_np.group(1))
            m_ca = pattern_ca.search(line)
            if m_ca:
                ca_funcs.append(m_ca.group(1))
            m_vac = pattern_vac.search(line)
            if m_vac:
                vac_funcs.append(m_vac.group(1))

        for func in vac_funcs:
            func_clean = clean_func_name('ls', func)
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'el', 'func': func_clean}
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'su', 'func': func_clean}
        for func in np_funcs:
            func_clean = clean_func_name('ls', func)
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'su', 'func': func_clean}
        for func in ca_funcs:
            func_clean = clean_func_name('ls', func)
            df_bug.loc[len(df_bug)] = {'id': id, 'kind': 'el', 'func': func_clean}

    assert len(df) == len(df_meta)
    return df, df_bug

def extract_from_slither(df_meta: pd.DataFrame, output_path, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
        'line': []
    })
    
    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        result_path = os.path.join(output_path, id, '.json')

        if not os.path.isfile(result_path) or os.path.getsize(result_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        try:
            with open(result_path, 'r') as f:
                j = json.load(f)
        except Exception:
            assert False

        if 'error' in j and j['error']:
            print(j['error'])
            assert False
            
        df.loc[i] = {'id': id, 'succeed': True}

        results = j['results']
        if len(results) == 0:
            continue
        alarms = j['results']['detectors']
        for alarm in alarms:
            vtyp = alarm['check']
            func_elements = [e for e in alarm['elements'] if e['type'] == 'function']
            node_elements = [e for e in alarm['elements'] if e['type'] == 'node']
            lines = [e["source_mapping"]['lines'][0] for e in node_elements]
            assert len(func_elements) == 1
            func_elem = func_elements[0]
            fname = func_elem['name']
            if not fname:
                assert False
            if fname == 'fallback':
                fname = '@fallback'

            kind = (
                'el' if vtyp == 'arbitrary-send-eth' else
                'su' if vtyp == 'suicidal' else
                're' if vtyp == 'reentrancy-eth' else ''
            )
            if kind not in bug_types:
                assert False
                
            assert kind == 'su' or len(lines) >= 0
            if kind == 'el':
                for line in lines:
                    df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': fname, 'line': str(line)}
            else:
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': fname, 'line': ""}

    assert len(df) == len(df_meta)
    return df, df_bug

def extract_from_sailfish(df_meta: pd.DataFrame, dataset: str, output_path):
    assert dataset == 're'
    tool = 'sailfish'
    
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })

    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        log_path = os.path.join(dir_path, id, 'contractlint.log')
        if not os.path.isfile(log_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        try:
            with open(log_path, encoding='utf-8') as fp:
                lines = fp.readlines()
            last_nonempty = next((line for line in reversed(lines) if line.strip()), '')
            if 'Analysis finished at:' not in last_nonempty:
                df.loc[i] = {'id': id, 'succeed': False}
                continue
        except Exception:
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        nested_dir = os.path.join(dir_path, id)
        if not os.path.isdir(nested_dir):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        json_files = [
            os.path.join(nested_dir, f)
            for f in os.listdir(nested_dir)
            if f.startswith('dao_sympath_') and f.endswith('.json')
        ]

        if not json_files:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        df.loc[i] = {'id': id, 'succeed': True}

        for jf in json_files:
            try:
                with open(jf, encoding='utf-8') as jfp:
                    data = json.load(jfp)
                if data.get('result') != 'SAT':
                    continue
                func = data.get('to_function', '').split('(')[0]
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': 're', 'func': func}
            except Exception:
                assert False

    assert len(df) == len(df_meta)
    return df, df_bug

def extract_from_slise(df_meta: pd.DataFrame, dataset: str, output_path):
    assert dataset == 're'
    
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })

    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        sol_file_path = os.path.join(f'{BENCH_DIR}/contracts/re', f'{id}.sol')
        compiler_version = get_solv_from_str(df_meta.loc[i]['compiler_version'], minimum_required='0.4.16')
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        json_path = os.path.join(dir_path, 'vulnerabilities.json')
        if not os.path.isfile(json_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue

        try:
            with open(json_path, encoding='utf-8') as fp:
                data = json.load(fp)
        except Exception:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        df.loc[i] = {'id': id, 'succeed': True}
        
        for file_entry in data:
            if id not in file_entry.get('filename'):
                continue
            for vuln in file_entry.get('vulnerabilities', []):
                if vuln.get('type') != 'Reentrancy':
                    assert False
                func = vuln.get('function_name')
                if '(' in func:
                    func = func.split('(')[0]
                try:
                    func = clean_func_name('re', func)
                except IndexError:
                    print(func)
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': 're', 'func': func}
    
    assert len(df) == len(df_meta)
    return df, df_bug


def extract_from_confuzzius(df_meta: pd.DataFrame, dataset: str, output_path: str, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
        'line': []
    })
    
    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        json_path = os.path.join(output_path, id, f'{id}.json')
        if not os.path.isfile(json_path) or os.path.getsize(json_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        with open(json_path, 'r') as fp:
            j = json.load(fp)
            
        df.loc[i] = {'id': id, 'succeed': True}
        
        assert len([x for x in j]) == 1
        j = j[[x for x in j][0]]
        # keys: 'errors', 'generations', 'transactions', 'code_coverage', 
        # 'branch_coverage', 'execution_time', 'memory_consumption', 
        # 'address_under_test', 'seed'
        errors = j['errors']
        for eid in errors:
            error = errors[eid]
            for e in error:
                kind = e['type']
                kind = (
                    'el' if kind == 'Leaking Ether' else
                    'su' if kind == 'Unprotected Selfdestruct' else
                    'io' if kind == 'Integer Overflow' else
                    're' if kind == 'Reentrancy' else 
                    ''
                )
                if kind not in bug_types:
                    continue
                if 'line' not in e:
                    print(f'{id}')
                    continue
                line = e['line']
                # func = e['individual'][-1]['transaction']['data'][2:10]
                # if len(func) < 8:
                #     func = '@fallback'
                # else:
                #     func = find_func(compiler_version, sol_file_path, func)
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': '', 'line': str(line)}
                
    assert len(df) == len(df_meta)
    return df, df_bug
        


def extract_from_efcf(df_meta: pd.DataFrame, output_path: str, bug_types):
    df = pd.DataFrame({
        'id': [],
        'succeed': [],
    })
    df_bug = pd.DataFrame({
        'id': [],
        'kind': [],
        'func': [],
    })
    
    for i in df_meta.index:
        id = str(df_meta['id'][i])
        dir_path = os.path.join(output_path, id)
        if not os.path.isdir(dir_path):
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        stdout_path = os.path.join(output_path, id, '.stdout.txt')
        if not os.path.isfile(stdout_path) or os.path.getsize(stdout_path) == 0:
            df.loc[i] = {'id': id, 'succeed': False}
            continue
        
        df.loc[i] = {'id': id, 'succeed': True}
        
        with open(stdout_path, 'r') as fp:
            s = fp.read()
            
        s = s.split('Transactions Sequences:\n--------------------------------------------------------------\n')
        assert len(s) <= 2
        if len(s) == 1:
            # no finding
            continue
        
        s = s[1]
        s = s.split('--------------------------------------------------------------')
        assert len(s) >= 2
        s = s[0].strip()
        
        l = s.split('TX [')
        assert len(l[0]) == 0
        l = l[1:]
        l = [tseq.rstrip().split('\n')[-1].lstrip() for tseq in l] # get last tx for each tseq
        for tx in l:
            assert '(' in tx
        l = [tx.split('(')[0] for tx in l]
        l = sorted(set(l))
        for func_name in l:
            func = '@fallback' if func_name == 'fallback' else func_name
            for kind in bug_types:
                df_bug.loc[len(df_bug)] = {'id': id, 'kind': kind, 'func': func}
                
    assert len(df) == len(df_meta)
    return df, df_bug


"""
Utilities
"""


def postproc(df_: pd.DataFrame, df_bug_: pd.DataFrame, bug_kinds: list[str]) -> pd.DataFrame:
    df = df_.copy()
    df_bug = df_bug_.copy() # id, kinc, func?, line?
    
    for kind in bug_kinds:
        df_bug_curr = df_bug[df_bug['kind'] == kind]
        if 'func' in df_bug_curr.keys():
            df_right = df_bug_curr[['id', 'func']] \
                       .drop_duplicates().groupby(by='id').aggregate("/".join) \
                       .rename(columns={'func': f'{kind}-f'})
            df = df.merge(df_right, on='id', how='left')
        if 'line' in df_bug_curr.keys():
            df_right = df_bug_curr[['id', 'line']] \
                       .drop_duplicates().groupby(by='id').aggregate("/".join) \
                       .rename(columns={'line': f'{kind}'})
            df = df.merge(df_right, on='id', how='left')
        
    return df
        
def reassign_succeed(df: pd.DataFrame):
    succeed = []
    cnt = 0
    for i in df.index:
        if df.loc[i]['succeed'] == True:
            cnt += 1
            succeed.append(cnt)
        else:
            succeed.append(None)
    df_ = df.copy()
    df_['succeed'] = succeed
    return df_

def analyze_main(args: argparse.Namespace, df):
    tool = args.tool
    dataset = args.dataset
    result_dir = f'result/{dataset}.{tool}'
    df_ret = extract_findings(df, tool, dataset, result_dir)
    df_ret = reassign_succeed(df_ret)
    df_ret.to_csv(f'{RESULT_DIR}/summary/{dataset}/{tool}.csv', index=False)
    
# other utils
def see_stderr(args: argparse.Namespace):
    tool = args.tool
    dataset = args.dataset
    result_dir = f'result/{dataset}.{tool}'
    l = sorted(os.listdir(result_dir))
    for dirname in l:
        stderr_name = os.path.join(result_dir, dirname, '.stderr.txt')
        if not os.path.isfile(stderr_name) or os.path.getsize(stderr_name) == 0:
            continue
        with open(stderr_name, 'r') as fp:
            s = fp.read()
        print(f'--- stderr for {dirname} ---')
        print(s[:200])
        print()
        print()
        
        
# ----- experiment result compressor -----
        
        
def compress_smartian(r, w):
    state = 0
    while True:
        line: str = r.readline()
        if not line:
            assert state == 0
            break
        if state == 0:
            if line.startswith('Inconsistent stack @ '):
                state = 1
            else:
                w.write(line)
        elif state == 1:
            state = 0
        else:
            assert False
            

def compress_lent(w):
    print('<stdout omitted due to large file size>', file=w)
    
    
def compress_confuzzius(w):
    print('<stderr omitted due to large file size>', file=w)
    
    
def compress_stdout(args: argparse.Namespace, df: pd.DataFrame):
    if args.tool not in ['smartian', 'lent']:
        return
    for i in df.index:
        id = df.loc[i]['id']
        readpath = f'{RESULT_DIR}/{args.dataset}/{args.tool}/{id}/.stdout.txt'
        writepath = f'{RESULT_DIR}/{args.dataset}/{args.tool}/{id}/.stdout.rep.txt'
        if not os.path.isfile(readpath) or os.path.getsize(readpath) < 1048576:
            continue
        print(f'compressing {id}')
        with open(readpath, 'r') as fp_read, open(writepath, 'w') as fp_write:
            print('<compressed large file>', file=fp_write)
            if args.tool == 'smartian':
                compress_smartian(fp_read, fp_write)
            elif args.tool == 'lent':
                compress_lent(fp_write)
        os.system(f'mv {writepath} {readpath}')
        
def compress_stderr(args: argparse.Namespace, df: pd.DataFrame):
    if args.tool not in ['confuzzius']:
        return
    for i in df.index:
        id = df.loc[i]['id']
        readpath = f'{RESULT_DIR}/{args.dataset}/{args.tool}/{id}/.stderr.txt'
        writepath = f'{RESULT_DIR}/{args.dataset}/{args.tool}/{id}/.stderr.rep.txt'
        if not os.path.isfile(readpath) or os.path.getsize(readpath) < 1048576:
            continue
        print(f'compressing {id}')
        with open(readpath, 'r') as fp_read, open(writepath, 'w') as fp_write:
            print('<compressed large file>', file=fp_write)
            if args.tool == 'confuzzius':
                compress_confuzzius(fp_write)
        os.system(f'mv {writepath} {readpath}')
        
def compress(args: argparse.Namespace, df: pd.DataFrame):
    compress_stdout(args, df)
    compress_stderr(args, df)
    
    
# ----- experiment result compressor end -----

# ----- grader -----

def _meta(dataset):
    df = pd.read_csv(f"{BENCH_DIR}/meta/{dataset}.csv", dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    return df

def _ground(dataset):
    df = pd.read_csv(f"{BENCH_DIR}/labels/{dataset}.csv", dtype=np.object_)
    return df

def _count(dataset: str, kind: str, tool: str, func, common=False) -> tuple[int, int, int]:
    """
    read a bug report and count
    """
    is_func = "-f" if func == 'func' else "" if func == 'line' else exit(1)
    df_ret = pd.read_csv(f'{RESULT_DIR}/summary/{dataset}/{tool}.csv', dtype=np.object_)
    if dataset == 'ls':
        df_candid = pd.read_csv(f'{BENCH_DIR}/misc/candidates-{kind}.csv', dtype=np.object_)
    if common:
        df_common = pd.read_csv(f'{RESULT_DIR}/summary/{dataset}/common.csv', dtype=np.object_)
        df_ret = df_common.merge(df_ret, how='left', on='id')
    df_ground = _ground(dataset)
    df = df_ret.merge(df_ground, how='left', on='id', suffixes=[f'-{tool}', ''])
    if dataset == 'ls':
        df = df.merge(df_candid, how='left', on='id')
    
    kind = kind.lower()
    tp = 0
    non_gt = 0
    fp = 0
    
    unknown_fps = []
    for i in df.index:
        id = df['id'][i]
        if df[f'{kind}{is_func}'].isna()[i]:
            known_tp = []
        elif df.loc[i][f'{kind}{is_func}'] == "-":
            known_tp = []
        else:
            known_tp = df[f'{kind}{is_func}'][i].split('/')
        if f'fp-{kind}{is_func}' not in df.keys() or df[f'fp-{kind}{is_func}'].isna()[i]:
            known_fp = []
        else:
            known_fp = df[f'fp-{kind}{is_func}'][i].split('/')
        if tool == 'efcf': 
            # do this since we could not distinguish el alarms and su alarms in efcf
            if kind == 'el':
                if df[f'su-f'].isna()[i]:
                    known_tp_opposite = []
                else:
                    known_tp_opposite = df[f'su-f'][i].split('/')
            elif kind == 'su':
                if df[f'el-f'].isna()[i]:
                    known_tp_opposite = []
                else:
                    known_tp_opposite = df[f'el-f'][i].split('/')
            else:
                known_tp_opposite = []
            known_tp_opposite2 = []
            for known_tp_opposite_elem in known_tp_opposite:
                known_tp_opposite_elem = known_tp_opposite_elem.split(':')
                known_fp.extend(known_tp_opposite_elem)
                known_tp_opposite2.extend(known_tp_opposite_elem)
        if df[f'{kind}{is_func}-{tool}'].isna()[i]:
            bugs = []
        else:
            bugs = df[f'{kind}{is_func}-{tool}'][i].split('/')
        if dataset != 'ls' or isinstance(df.loc[i]['candidates'], float):
            candidates = None
        elif df.loc[i]['candidates'] == '-':
            candidates = []
        else:
            candidates = df.loc[i]['candidates'].split('/')
        for f_name in bugs:
            for j in range(len(known_tp)):
                if f_name in known_tp[j].split(':'):
                    tp += 1
                    break
            else:
                non_gt += 1
                if f_name in known_fp:
                    fp_pass = False
                    if tool == 'efcf' and f_name in known_tp_opposite2:
                        fp_pass = True
                    if tool == 'efcf' and f_name == '@fallback':
                        fp_pass = True
                    if fp_pass == False:
                        fp += 1
                if f_name not in known_fp and dataset != 'io' and not common:
                    if tool == 'efcf' and f_name == '@fallback':
                        continue
                    if tool == 'achecker':
                        if candidates is not None and f_name not in candidates:
                            continue
                    unknown_fps.append(f'{id} (ground: {known_tp}) {f_name}')
    if len(unknown_fps) > 0:
        print(f'========== unknown alarms for {tool}-{dataset}-{kind} ==========')
        for s in unknown_fps:
            print(s)     
        print()       
                
    return tp, non_gt, fp


def count_wrap(df: pd.DataFrame, dataset, kind, tool, func, common):
    tp, nongt, fp = _count(dataset, kind, tool, func, common)
    colname = kind
    colname += '-f' if func == 'func' else ''
    colname += '-com' if common == True else ''
    df.loc[tool, colname] = tp
    if dataset != 'io' and common == False:
        df.loc[tool, colname + '-fp'] = fp
    if common == False:
        df.loc[tool, colname + '-nongt'] = nongt
    
    
def compute_total(df: pd.DataFrame, tool):
    df.loc[tool, 'total-f'] = \
        df.loc[tool]['el-f'] + df.loc[tool]['su-f'] + df.loc[tool]['io-f'] + df.loc[tool]['re-f']
    df.loc[tool, 'total'] = \
        df.loc[tool]['el'] + df.loc[tool]['su'] + df.loc[tool]['io'] + df.loc[tool]['re']
    df.loc[tool, 'total-f-fp'] = \
        df.loc[tool]['el-f-fp'] + df.loc[tool]['su-f-fp'] + df.loc[tool]['re-f-fp']
    df.loc[tool, 'total-fp'] = \
        df.loc[tool]['el-fp'] + df.loc[tool]['su-fp'] + df.loc[tool]['re-fp']
    if tool == 'slither':
        df.loc[tool, 'total'] = 0
        df.loc[tool, 'total-fp'] = 0
    
    
def compute_precision(df: pd.DataFrame, tool: str, kind: str):
    if df.loc[tool][kind] > 0:
        tp = df.loc[tool][kind]
        if kind == 'total':
            tp -= df.loc[tool]['io']
        fp = df.loc[tool][kind + '-fp']
        precision = tp / (tp + fp)
    elif df.loc[tool][kind + '-f'] > 0:
        tp = df.loc[tool][kind + '-f']
        if kind == 'total':
            tp -= df.loc[tool]['io-f']
        fp = df.loc[tool][kind + '-f-fp']
        precision = tp / (tp + fp)
    else:
        precision = 0
    df.loc[tool, kind + '-precision'] = precision
        

def main():
    if len(sys.argv) == 1:
        do_all()
        exit(0)
        
    parser = argparse.ArgumentParser()
    
    # main arguments
    parser.add_argument('-t', '--tool', type=str, required=True,
                        help='(required) Analysis tool to use. e.g., mythril, slither, lent')
    parser.add_argument('-d', '--dataset', type=str, required=True, choices=['io', 'ls', 're'],
                        help='(required) Dataset name')
    parser.add_argument('--bench-dir', type=str, default=BENCH_DIR,
                        help='benchmark directory location')
    
    # util arguments
    parser.add_argument('--see-stderr', action='store_true', help='print all stderrs and exit')
    parser.add_argument('--compress', action='store_true', help='compress large stdout/stderr files')

    args = parser.parse_args()
    
    df = pd.read_csv(os.path.join(args.bench_dir, 'meta', f'{args.dataset}.csv'), dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    
    if args.see_stderr:
        return see_stderr(args)
    if args.compress:
        return compress(args, df)
    
    analyze_main(args, df)
    
    
def do_all():
    for tool in LS_TOOL:
        os.system(f'python scripts/analyze_tool.py --tool {tool} --dataset ls')
    df = pd.read_csv(os.path.join(BENCH_DIR, 'meta', 'ls.csv'), dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    df = df[['id']]
    df1 = df.copy()
    for tool in LS_TOOL:
        df2 = pd.read_csv(os.path.join(RESULT_DIR, 'summary', 'ls', f'{tool}.csv'), dtype=np.object_)
        df1 = df1.merge(df2, on='id', how='left')
        df1 = df1[~df1['succeed'].isna()]
        df1 = df1[['id']]
    df1.to_csv(f'{RESULT_DIR}/summary/ls/common.csv', index=False)
    
    for tool in IO_TOOL:
        os.system(f'python scripts/analyze_tool.py --tool {tool} --dataset io')
    df = pd.read_csv(os.path.join(BENCH_DIR, 'meta', 'io.csv'), dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    df = df[['id']]
    df1 = df.copy()
    for tool in IO_TOOL:
        df2 = pd.read_csv(os.path.join(RESULT_DIR, 'summary', 'io', f'{tool}.csv'), dtype=np.object_)
        df1 = df1.merge(df2, on='id', how='left')
        df1 = df1[~df1['succeed'].isna()]
        df1 = df1[['id']]
    df1.to_csv(f'{RESULT_DIR}/summary/io/common.csv', index=False)
    
    for tool in RE_TOOL:
        os.system(f'python scripts/analyze_tool.py --tool {tool} --dataset re')
    df = pd.read_csv(os.path.join(BENCH_DIR, 'meta', 're.csv'), dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    df = df[['id']]
    df1 = df.copy()
    for tool in RE_TOOL:
        df2 = pd.read_csv(os.path.join(RESULT_DIR, 'summary', 're', f'{tool}.csv'), dtype=np.object_)
        df1 = df1.merge(df2, on='id', how='left')
        df1 = df1[~df1['succeed'].isna()]
        df1 = df1[['id']]
    df1.to_csv(f'{RESULT_DIR}/summary/re/common.csv', index=False)
    
    # df_ret = pd.read_csv(os.path.join(RESULT_DIR, 'summary', 'table.csv'), index_col='tool')
    # df_ret = df_ret.fillna(0.0)
    df_ret = pd.DataFrame()
    
    count_wrap(df_ret, 'ls', 'el', 'smartrim', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'smartrim', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'smartrim', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'smartrim', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'smartrim', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'smartrim', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'smartrim', 'line', False)
    count_wrap(df_ret, 'ls', 'su', 'smartrim', 'line', True)
    count_wrap(df_ret, 'io', 'io', 'smartrim', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'smartrim', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'smartrim', 'line', False)
    count_wrap(df_ret, 'io', 'io', 'smartrim', 'line', True)
    count_wrap(df_ret, 're', 're', 'smartrim', 'func', False)
    count_wrap(df_ret, 're', 're', 'smartrim', 'func', True)
    count_wrap(df_ret, 're', 're', 'smartrim', 'line', False)
    count_wrap(df_ret, 're', 're', 'smartrim', 'line', True)
    compute_total(df_ret, 'smartrim')
    
    count_wrap(df_ret, 'ls', 'el', 'smartest', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'smartest', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'smartest', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'smartest', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'smartest', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'smartest', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'smartest', 'line', False)
    count_wrap(df_ret, 'ls', 'su', 'smartest', 'line', True)
    count_wrap(df_ret, 'io', 'io', 'smartest', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'smartest', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'smartest', 'line', False)
    count_wrap(df_ret, 'io', 'io', 'smartest', 'line', True)
    compute_total(df_ret, 'smartest')
    
    count_wrap(df_ret, 'ls', 'el', 'efcf', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'efcf', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'efcf', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'efcf', 'func', True)
    count_wrap(df_ret, 're', 're', 'efcf', 'func', False)
    count_wrap(df_ret, 're', 're', 'efcf', 'func', True)
    compute_total(df_ret, 'efcf')
    
    count_wrap(df_ret, 'ls', 'el', 'smartian', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'smartian', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'smartian', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'smartian', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'smartian', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'smartian', 'func', True)
    count_wrap(df_ret, 're', 're', 'smartian', 'func', False)
    count_wrap(df_ret, 're', 're', 'smartian', 'func', True)
    compute_total(df_ret, 'smartian')
    
    count_wrap(df_ret, 'ls', 'el', 'confuzzius', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'confuzzius', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'confuzzius', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'confuzzius', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'confuzzius', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'confuzzius', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'confuzzius', 'line', False)
    count_wrap(df_ret, 'ls', 'su', 'confuzzius', 'line', True)
    count_wrap(df_ret, 'io', 'io', 'confuzzius', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'confuzzius', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'confuzzius', 'line', False)
    count_wrap(df_ret, 'io', 'io', 'confuzzius', 'line', True)
    count_wrap(df_ret, 're', 're', 'confuzzius', 'func', False)
    count_wrap(df_ret, 're', 're', 'confuzzius', 'func', True)
    count_wrap(df_ret, 're', 're', 'confuzzius', 'line', False)
    count_wrap(df_ret, 're', 're', 'confuzzius', 'line', True)
    compute_total(df_ret, 'confuzzius')
    
    count_wrap(df_ret, 'ls', 'el', 'rlf', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'rlf', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'rlf', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'rlf', 'func', True)
    compute_total(df_ret, 'rlf')
    
    count_wrap(df_ret, 'ls', 'el', 'mythril', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'mythril', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'mythril', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'mythril', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'mythril', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'mythril', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'mythril', 'line', False)
    count_wrap(df_ret, 'ls', 'su', 'mythril', 'line', True)
    count_wrap(df_ret, 'io', 'io', 'mythril', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'mythril', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'mythril', 'line', False)
    count_wrap(df_ret, 'io', 'io', 'mythril', 'line', True)
    count_wrap(df_ret, 're', 're', 'mythril', 'func', False)
    count_wrap(df_ret, 're', 're', 'mythril', 'func', True)
    compute_total(df_ret, 'mythril')
    
    count_wrap(df_ret, 'ls', 'el', 'lent', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'lent', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'lent', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'lent', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'lent', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'lent', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'lent', 'line', False)
    count_wrap(df_ret, 'ls', 'su', 'lent', 'line', True)
    count_wrap(df_ret, 'io', 'io', 'lent', 'func', False)
    count_wrap(df_ret, 'io', 'io', 'lent', 'func', True)
    count_wrap(df_ret, 'io', 'io', 'lent', 'line', False)
    count_wrap(df_ret, 'io', 'io', 'lent', 'line', True)
    count_wrap(df_ret, 're', 're', 'lent', 'func', False)
    count_wrap(df_ret, 're', 're', 'lent', 'func', True)
    compute_total(df_ret, 'lent')
    
    count_wrap(df_ret, 'ls', 'el', 'slither', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'slither', 'func', True)
    count_wrap(df_ret, 'ls', 'el', 'slither', 'line', False)
    count_wrap(df_ret, 'ls', 'el', 'slither', 'line', True)
    count_wrap(df_ret, 'ls', 'su', 'slither', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'slither', 'func', True)
    count_wrap(df_ret, 're', 're', 'slither', 'func', False)
    count_wrap(df_ret, 're', 're', 'slither', 'func', True)
    count_wrap(df_ret, 're', 're', 'slither', 'line', False)
    count_wrap(df_ret, 're', 're', 'slither', 'line', True)
    compute_total(df_ret, 'slither')
    
    count_wrap(df_ret, 'ls', 'el', 'achecker', 'func', False)
    count_wrap(df_ret, 'ls', 'el', 'achecker', 'func', True)
    count_wrap(df_ret, 'ls', 'su', 'achecker', 'func', False)
    count_wrap(df_ret, 'ls', 'su', 'achecker', 'func', True)
    compute_total(df_ret, 'achecker')
    
    count_wrap(df_ret, 're', 're', 'slise', 'func', False)
    count_wrap(df_ret, 're', 're', 'slise', 'func', True)
    compute_total(df_ret, 'slise')
    
    count_wrap(df_ret, 're', 're', 'sailfish', 'func', False)
    count_wrap(df_ret, 're', 're', 'sailfish', 'func', True)
    compute_total(df_ret, 'sailfish')
    
    for kind in ['el', 'su', 're', 'total']:
        for tool in TOOL:
            compute_precision(df_ret, tool, kind)
    
    #df_ret.reindex(columns="tool,el-f,el,el-f-com,el-com,su-f,su,su-f-com,su-com,io-f,io,io-f-com,io-com,re-f,re,re-f-com,re-com,total-f,total,el-f-fp,el-fp,su-f-fp,su-fp,re-f-fp,re-fp,total-f-fp,total-fp,el-precision,su-precision,re-precision,total-precision,el-f-nongt,el-f-com-nongt,el-nongt,el-com-nongt,su-f-nongt,su-f-com-nongt,su-nongt,su-com-nongt,io-f-nongt,io-f-com-nongt,io-nongt,io-com-nongt,re-f-nongt,re-f-com-nongt,re-nongt,re-com-nongt".split(','))
    df_ret.map(df_format_number).to_csv(os.path.join(RESULT_DIR, 'summary', 'table.csv'))
    

if __name__ == '__main__':
    main()