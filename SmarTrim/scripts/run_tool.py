import sys
import os
import time
import csv
import json
import subprocess
from datetime import datetime
import argparse
from multiprocessing import Pool, Lock, Value
from os.path import expanduser

# TOOLS = ['slither', 'smartest', 'mythril']
# TOOLS = ['slither', 'mythril', 'verismart', 'smartest']
TOOLS = ['verismart']

FIELDNAMES = ['no', 'last', 'vultyp', 'cname', 'signature', 'line', 'exp',
              'final'] + TOOLS

TOOL_TIMEOUT = 0
Z3_TIMEOUT = 0
ALARM_BOUND = 0
SOLV = ""
MAIN_CNAME = ""
CEI_VOIOLATED = False
BIT_REDUC = False

def empty_fieldnames():
    return dict.fromkeys(FIELDNAMES, '')


def rawdir_in(rootdir):
    return os.path.join(rootdir, 'raw')


def csvdir_in(rootdir):
    return os.path.join(rootdir, 'csv')


def patdir_in(rootdir):
    return os.path.join(rootdir, 'candidates')


def logdir_in(rootdir):
    return os.path.join(rootdir, 'log')


def vulreport_in(rootdir):
    return os.path.join(rootdir, 'vulnerability_report.csv')

def regreport_in(rootdir):
    return os.path.join(rootdir, 'regression_report.csv')

def get_cmd(tool, inputfile, rawdir):
    cmd = []
    if tool=='smartest':
        cmd = ['./main.native', '-input', inputfile, '-main', MAIN_CNAME,
               '-solc', SOLV,
               '-mode', 'exploit', '-refined_vcgen',
               'io', 'kill', 'leak',
               '-contract_init_eth', '10',
               '-exploit_timeout', str(TOOL_TIMEOUT) + ';',
               'mv',
               os.path.join('validation-files', os.path.basename(inputfile).replace('.sol','') + '_*'), # without '_', e.g., 'X16_..' will be moved when moving 'X1_..'
               rawdir]
    elif tool=='verismart':
        os.path.basename(inputfile)
        cmd = ['./main.native', '-input', inputfile, '-main', MAIN_CNAME, '-json',
               '-solv', SOLV, '-verbose',
               '-mode', 'verify', '-refined_vcgen', '-uninterp_nonlinear', '-cond_safe',
               'io', 'kill', 'leak', 're', 'tx',
               'reg',
               '-verify_timeout', str(TOOL_TIMEOUT),
               '-z3timeout', str(Z3_TIMEOUT),
               '-alarm_bound' if ALARM_BOUND > 0 else '',
               str(ALARM_BOUND) if ALARM_BOUND > 0 else '',
               '-fast_verify' if BIT_REDUC else '',
               '-re_safety_enforce' if CEI_VIOLATED else '',
               '-outdir', rawdir + ';',
               'mv',
               os.path.join(rawdir, os.path.basename(inputfile).replace('.sol','.json')),
               os.path.join(rawdir, 'verismart.json')]
    elif tool=='slither':
        cmd = ['slither', inputfile, '--json', os.path.join(rawdir, 'slither.txt'),
               '--detect', 'arbitrary-send,suicidal,reentrancy-eth',
               '--solc-disable-warnings']
    elif tool=='mythril':
        container_name = 'mythril' + '_' + os.path.basename(inputfile).replace('.sol', '')
        cmd = ['docker', 'create', '--entrypoint=/bin/bash', '--name', container_name, '-it', 'mythril/myth:0.22.14',
               ';', 'docker', 'start', container_name,
               ';', 'docker', 'exec', container_name, 'mkdir', '/root/.py-solc',
               ';', 'docker', 'exec', container_name, 'mkdir', f'/root/.py-solc/solc-v{SOLV}',
               ';', 'docker', 'exec', container_name, 'mkdir', f'/root/.py-solc/solc-v{SOLV}/bin',
               ';', 'docker', 'cp', f'/usr/local/bin/solc_{SOLV}', f'{container_name}:/root/.py-solc/solc-v{SOLV}/bin/solc',
               ';', 'docker', 'cp', inputfile, f'{container_name}:/tmp/',
               ';', 'docker', 'exec', container_name, 'myth', 'analyze', '/tmp/'+os.path.basename(inputfile)+':'+MAIN_CNAME,
                    '--execution-timeout', str(TOOL_TIMEOUT),
                    '--solv', SOLV, '-t', '4',
                    '--modules', 'IntegerArithmetics,EtherThief,AccidentallyKillable,StateChangeAfterCall', # ExternalCalls' <= imprecise w.r.t. exploitability
                    '-o', 'json',
                    '>', os.path.join(rawdir, 'mythril.txt'),
               ';', 'docker', 'stop', container_name,
               ';', 'docker', 'rm', container_name]
        # cmd = ['docker', 'run', '--rm', '-v', ...':/tmp:ro',
        #        'mythril/myth:0.22.14', 'analyze', f'/tmp/{inputfile}:' + MAIN_CNAME,
        #        '--execution-timeout', str(TOOL_TIMEOUT), '-t', '4', '-o', 'json',
        #        '--modules', 'IntegerArithmetics,EtherThief,AccdidentallyKillable']
    else:
        assert(False)
    return cmd


def run(tool, inputfile, outroot):
    cmd = get_cmd (tool,inputfile,rawdir_in(outroot))
    cmd = " ".join(cmd)
    p = subprocess.Popen (cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, shell=True)
    stdout = p.stdout.read()
    with open(os.path.join(logdir_in(outroot), tool+'.txt'), 'w') as fp:
        fp.write(stdout.decode('utf-8'))
    with open(os.path.join(logdir_in(outroot), tool+'_cmd.txt'), 'w') as fp:
        fp.write(cmd)


def normalize_checker (checker):
    # slither
    if checker=='reentrancy-eth':
        return 'RE_EL'
    elif checker=='suicidal':
        return 'KA'
    elif checker=='arbitrary-send':
        return 'ETH_LEAK'
    # verismart, smartest
    elif checker=='IO':
        return 'IO'
    elif checker=='ETH_LEAK':
        return 'ETH_LEAK'
    elif checker=='KA':
        return 'KA'
    elif checker=='NO_EFFECT':
        return 'NO_EFFECT'
    elif checker=='ASSIGN_CONST':
        return 'ASSIGN_CONST'
    elif checker=='RE_EL':
        return 'RE_EL'
    elif checker=='RE':
        return 'RE'
    elif checker=='TX_ORG':
        return 'TX_ORG'
    # mythril
    elif checker=='Unprotected Ether Withdrawal':
        return 'ETH_LEAK'
    elif checker=='Unprotected Selfdestruct':
        return 'KA'
    elif checker=='Integer Arithmetic Bugs':
        return 'IO'
    elif checker=='State access after external call':
        return 'RE'
    elif checker=='External Call To User-Supplied Address':
        return 'RE'
    else:
        raise NotImplementedError


def parse_slither(rawfile):
    with open(rawfile, 'r') as fp: j = json.load(fp)

    result = []

    if len(j['results']) == 0:
        return result

    vuls = j['results']['detectors']
    for (i,v) in enumerate(vuls):
        checker = normalize_checker(v['check'])
        elems = v['elements']
        elems = list(filter (lambda e: e['type']!='function', elems))
        elems = list(filter (lambda e: e['additional_fields']['underlying_type']=='external_calls', elems)) if checker=='RE_EL' else elems
        for (j,e) in enumerate(elems):
            cname = e['type_specific_fields']['parent']['type_specific_fields']['parent']['name']
            fsig = e['type_specific_fields']['parent']['type_specific_fields']['signature']
            lines = e['source_mapping']['lines']
            assert(e['type'] == 'node')
            assert(len(lines) == 1)
            result.append({
                'no': i+1,
                'last': 'O' if j+1==len(elems) else '-',
                'vultyp': checker,
                'cname': cname,
                'signature': fsig.replace(",","_"),
                'line': lines[0],
                'exp': 'none'
            })
    return result


def choose_re_kind(vuls):
    re1_qs = list(filter(lambda v: v['kind']=='RE', vuls))
    re2_qs = list(filter(lambda v: v['kind']=='RE_EL', vuls))

    re1_alarms = list(filter(lambda v: v['kind']=='RE' and v['status']=='unproven', vuls))
    re2_alarms = list(filter(lambda v: v['kind']=='RE_EL' and v['status']=='unproven', vuls))

    if len(re1_qs)==0:
        return 'RE_EL'
    elif len(re2_qs)==0: # modifier_reentrancy.sol
        return 'RE'
    elif len(re1_alarms) > len(re2_alarms):
        return 'RE_EL'
    else:
        return 'RE'


def parse_verismart(rawfile):
    with open(rawfile, 'r') as fp: j = json.load(fp)

    result = []

    vuls = j['vul_result']

    re_kind = choose_re_kind(vuls)
    no_re = 'RE' if re_kind=='RE_EL' else 'RE_EL'

    def pred(v):
      b1 = (v['kind']=='NO_EFFECT' or v['kind']=='ASSIGN_CONST') and v['status']=='proven'
      b2 = (v['kind']!='NO_EFFECT' and v['kind']!='ASSIGN_CONST' and v['kind']!=no_re) and v['status']=='unproven'
      return (b1 or b2)

    vuls = list(filter(lambda v: pred(v), vuls))
    for (i,v) in enumerate(vuls):
        checker = normalize_checker(v['kind'])
        line = v['line']
        exp = v['exp'] # 'none'
        signatures = v['signatures']
        assert(len(signatures) > 0)
        for (j,s) in enumerate(signatures):
            cname = s['contractName']
            # fname = s['methodName']
            # typs = '(' + '_'.join(s['argTypes']) + ')'
            result.append({
                'no': i+1,
                'last': 'O',
                'vultyp': checker,
                'cname': cname,
                'signature': '', # fname + typs,
                'line': line,
                'exp': exp
            })

    return result


def parse_smartest(rawdir):
    vuls = [f for f in os.listdir(rawdir) if f.endswith('.json')]

    result = []
    for (i,v) in enumerate(vuls):
        with open(os.path.join(rawdir,v), 'r') as fp: j = json.load(fp)
        checker = normalize_checker(j['kind'])
        line = j['line']
        tx_seq = j['tx_seq']
        for (j,tx) in enumerate(tx_seq):
            cname = tx['contractName']
            fname = tx['methodName']
            typs = '(' + '_'.join(tx['argTypes']) + ')'
            exp = 'none'
            result.append({
                'no': i+1,
                'last': 'O' if j+1==len(tx_seq) else '-',
                'vultyp': checker,
                'cname': cname,
                'signature': fname + typs,
                'line': line if j+1==len(tx_seq) else 'none',
                'exp': exp
            })
    return result


def parse_mythril(rawfile):
    try:
        with open(rawfile, 'r') as fp: j = json.load(fp)
    except FileNotFoundError:
        print('[WARNING:parse_mythril] file does not exist : ' + rawfile)
        return [] # empty (no) result for error case
    except json.decoder.JSONDecodeError:
        print('[WARNING:parse_mythril] json file is ill-formed : ' + rawfile)
        return [] # empty (no) result for error case

    result = []
    vuls = j['issues']
    for (i,v) in enumerate(vuls):
        checker = normalize_checker(v['title'])
        cname = v['contract']
        line = v['lineno']
        tx_seq = v['tx_sequence']['steps']
        for (j,tx) in enumerate(tx_seq):
            if tx['name'] == 'unknown': # XXX
                continue
            fsig = tx['name']
            exp = 'none'
            result.append({
                'no': i+1,
                'last': 'O' if j+1==len(tx_seq) else '-',
                'vultyp': checker,
                'cname': cname,
                'signature': fsig.replace(',','_'),
                'line': line if j+1==len(tx_seq) else 'none',
                'exp': exp
            })
    return result


def parse(tool,outroot):
    if tool=='smartest':
        result = parse_smartest(rawdir_in(outroot))
    elif tool=='verismart':
        result = parse_verismart(os.path.join(rawdir_in(outroot), tool+'.json'))
    elif tool=='slither':
        result = parse_slither(os.path.join(rawdir_in(outroot), tool+'.txt'))
    elif tool=='mythril':
        result = parse_mythril(os.path.join(rawdir_in(outroot), tool+'.txt'))
    else:
        assert(False)
    with open (os.path.join(csvdir_in(outroot), tool+'.csv'), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(result)


def abstract_row(row):
    return (row['vultyp'], row['cname'], row['signature'], row['line'], row['exp'])


def abstract_rows(rows):
    rows = list(filter(lambda r: r['last']=='O', rows))
    ret = set(map(lambda r: abstract_row(r), rows))
    return ret


def collect_all(outroot):
    result = set()
    for tool in TOOLS:
        with open(os.path.join(csvdir_in(outroot), tool+'.csv'), 'r') as fp: rows = list(csv.DictReader(fp))
        result = result.union(abstract_rows(rows))
    return result


def inspect_each_tool(outroot, total):
    result = []
    for (i,(vtyp,cname,fsig,line,exp)) in enumerate(total):
        per = empty_fieldnames()
        per['no'] = i+1
        per['last'] = 'O'
        per['vultyp'] = vtyp
        per['cname'] = cname
        per['signature'] = fsig
        per['line'] = line
        per['exp'] = exp
        for tool in TOOLS:
            with open(os.path.join(csvdir_in(outroot), tool+'.csv'), 'r') as fp: rows = list(csv.DictReader(fp))
            for row in rows:
                if abstract_row(row) == (vtyp,cname,fsig,line,exp): # TODO: devise abstract matching
                    per[tool] = 'O'
        result.append(per)

    return result


def reach_aggrement(outroot,oldrows):
    result = []
    for old in oldrows:
        detect = 0
        for tool in TOOLS:
            detect = detect+1 if old[tool]=='O' else detect
        verismart_detect = old['verismart']=='O' or old['smartest']=='O'
        tmp = old
        if detect >= 2 or verismart_detect:
            tmp['final'] = 'O'
            tmp['no'] = len(result) + 1
            result.append(tmp)
    return result


def find_related_idxs(processing, old, rows):
    rows = list(filter(lambda r: r['last']=='O' and abstract_row(old) == abstract_row(r), rows))
    idxs = list(map(lambda r: r['no'], rows))
    return idxs # due to line-level granularity, multiple indices may be found
    # if len(idxs)==0:
    #    return 'none'
    # else:
        # try:
        #    assert(len(set(idxs)) == 1) # unique index => unique vulnerability
        #    return idxs[0]
        # except AssertionError:
        #    print('[ERROR] len(set(idxs)) is not 1, but ' + str(len(set(idxs))) + ' : ' +  processing)
        #    exit(1)


def gather_related_funcs(outroot,oldrows):
    result = []
    for (i,old) in enumerate(oldrows):
        assert(str(old['no']) == str(i+1))
        result.append(old)
        abstracts = set()
        for tool in TOOLS:
            processing = os.path.join(csvdir_in(outroot), tool+'.csv')
            with open(processing, 'r') as fp: rows = list(csv.DictReader(fp))
            idxs = find_related_idxs(processing, old, rows)
            rows = list(filter(lambda r: r['no'] in idxs and r['last']!='O', rows))
            rows = set(map(lambda r: abstract_row(r), rows)) # do not use abstract_rows here
            abstracts = abstracts.union(rows)
        for (vtyp,cname,fsig,line) in abstracts:
            per = empty_fieldnames()
            per['no'] = i+1
            per['last'] = '-'
            per['vultyp'] = vtyp
            per['cname'] = cname
            per['signature'] = fsig
            per['line'] = line
            per['exp'] = 'none'
            result.append(per)
    return result


def generate_report(outroot):
    result = collect_all(outroot)
    result = inspect_each_tool(outroot,result)
    # result = reach_aggrement(outroot,result)
    result = gather_related_funcs(outroot,result)
    with open (vulreport_in(outroot), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(result)

def parse_regression(rawfile):
    with open(rawfile, 'r') as fp: j = json.load(fp)

    result = []

    regs = j['reg_result']

    def pred(r):
      b = (r['kind']=='NO_EFFECT' or r['kind']=='ASSIGN_CONST') and r['status']=='proven'
      return b

    regs = list(filter(lambda r: pred(r), regs))

    for (i,r) in enumerate(regs):
        checker = normalize_checker(r['kind'])
        line = r['line']
        exp = r['exp'] # 'none'
        signatures = r['signatures']
        assert(len(signatures) > 0)
        for (j,s) in enumerate(signatures):
            cname = s['contractName']
            result.append({
                'no': i+1,
                'last': 'O',
                'vultyp': checker,
                'cname': cname,
                'signature': '', # fname + typs,
                'line': line,
                'exp': exp
            })

    return result


def generate_reg_report(outroot):
    # with open(os.path.join(rawdir_in(outroot), 'verismart.json'), 'r') as fp: rows = list(csv.DictReader(fp))
   
    result = parse_regression (os.path.join(rawdir_in(outroot),'verismart.json'))

    # result = gather_related_funcs(outroot,result)
    with open (regreport_in(outroot), 'w') as fp:
        writer = csv.DictWriter(fp, fieldnames=FIELDNAMES)
        writer.writeheader()
        writer.writerows(result)


def setup_globals(tool_timeout, z3_timeout, solv, main_cname, cei_violated, bitreduc, alarm_bound):
    global TOOL_TIMEOUT
    global Z3_TIMEOUT
    global SOLV
    global MAIN_CNAME
    global CEI_VIOLATED
    global BIT_REDUC
    global ALARM_BOUND

    TOOL_TIMEOUT = tool_timeout
    Z3_TIMEOUT = z3_timeout
    SOLV = solv
    MAIN_CNAME = main_cname
    CEI_VIOLATED = cei_violated
    BIT_REDUC = bitreduc

    ALARM_BOUND = alarm_bound

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument ('--tool', type=str, help='{smartest,slither,mythril,verismart}')
    # parser.add_argument ('--main_name', type=str, help='specify main contract name')
    parser.add_argument ('--inputfile', type=str, help='input file name')
    parser.add_argument ('--outroot_dir', type=str, help='output root directory')
    parser.add_argument ('--tool_timeout', type=int, help='timeout in seconds', default=20)
    parser.add_argument ('--kill_timeout', type=int, default=1200)
    parser.add_argument ('--z3_timeout', type=int, default=30000)
    parser.add_argument ('--solv', type=str, help='solc version')
    parser.add_argument ('--main_cname', type=str, help='main contract name')
    parser.add_argument ('--alarm_bound', type=int, default=-1, help='alarm_bound')
    parser.add_argument ('--checker', type=str)
    parser.add_argument ('--cei_violated', default=False, action='store_true')
    parser.add_argument ('--bitreduc', default=False, action='store_true')
    parser.add_argument ('--generate_report', default=False, action='store_true')

    args = parser.parse_args()

    if args.generate_report:
        generate_report(args.outroot_dir)
        generate_reg_report(args.outroot_dir)
        return

    tool = args.tool
    inputfile = args.inputfile
    outroot = args.outroot_dir

    setup_globals(args.tool_timeout, args.z3_timeout, args.solv, args.main_cname, args.cei_violated, args.bitreduc, args.alarm_bound)

    assert (tool in TOOLS)

    run(tool,inputfile,outroot) # obtain raw report
    parse(tool,outroot) # generate prarsed results as CSV files

if __name__ == "__main__":
    main ()
