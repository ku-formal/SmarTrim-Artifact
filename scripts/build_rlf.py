#!/usr/bin/env python3

from utils import is_compilable_2

import os
import pandas as pd
import re

class DFA:
    def __init__(self):
        self.state = 0
        self.stack = []
        
    def _next(self, c: str):
        if self.state in [0, 1, 5, 6]:
            self.stack.append(c)
        if self.state == 0:
            if c == '/':
                self.state = 1
            elif c == '"':
                self.state = 5
        elif self.state == 1:
            if c == '/':
                self.state = 2
                self.stack.pop()
                self.stack.pop()
            elif c == '*':
                self.state = 3
                self.stack.pop()
                self.stack.pop()
            else:
                self.state = 0
        elif self.state == 2:
            if c == '\n':
                self.state = 0
                self.stack.append('\n')
        elif self.state == 3:
            if c == '*':
                self.state = 4
        elif self.state == 4:
            if c == '/':
                self.state = 0
            elif c == '*':
                self.state = 4
            else:
                self.state = 3
        elif self.state == 5:
            if c == '"':
                self.state = 0
            elif c == '\\':
                self.state = 6
        elif self.state == 6:
            self.state = 5
        else:
            raise ValueError("")
        
    def erase(self, s: str) -> str:
        for c in s:
            self._next(c)
        return "".join(self.stack)
    
    
def erase_comment(s: str) -> str:
    return DFA().erase(s)


HOME = os.path.expanduser('~')
DATASET = "ls"
CONTRACTS_PATH = os.path.join(HOME, "smartrim-benchmark", "contracts", "ls")
META_PATH = os.path.join(HOME, "smartrim-benchmark", "meta", "ls.csv")

def readfrom(filepath):
    with open(filepath, 'r') as f:
        s = f.read()
    return s

def writeto(filepath, s):
    with open(filepath, 'w') as f:
        f.write(s)

def remove_pragma_solidity(solidity_code):
    pattern = r'pragma\s+solidity\s+[^;]+;'
    result = re.sub(pattern, '', solidity_code)
    return result

def preprocessing(path: str, main_name: str):
    c = readfrom(path)
    c = remove_pragma_solidity(c)
    c = getconstructor(c, main_name)
    writeto(path, c)

def build_project(name: str, main_contract: str, origin_version: str):
    dir_name = f"{HOME}/smartrim-experiment/for-rlf/{name}"
    if os.path.exists(dir_name):
        print("Project path names collide. Abort")
        exit(1)
    os.system(f"mkdir {dir_name}")
    os.system(f"cp -r {HOME}/smartrim-experiment/assets/rlf-template/* {dir_name}")
    os.system(f"cat {dir_name}/truffle-config.js")
    os.system(f"cp {CONTRACTS_PATH}/{name}.sol {dir_name}/contracts")
    preprocessing(f"{dir_name}/contracts/{name}.sol", main_contract)
    v = ('0.4.25' if is_compilable_2('0.4.25', f"{dir_name}/contracts/{name}.sol") else 
         origin_version if origin_version.startswith('0.5') else 
         '0.4.22')
    j = os.system(f"python3 {HOME}/smartrim-experiment/assets/rlf-script/extract.py --proj {dir_name} --port 8545 --fuzz_contract {main_contract} --v {v}")
    return j

def get_ver_from_metastr(s: str) -> str:
    version_pattern = r'v\d\.\d\.\d{1,2}'
    version = re.findall(version_pattern, s)
    if len(version) == 0: return '0.8.20'
    return version[0][1:]
    
def build_project_all():
    meta = pd.read_csv(META_PATH)
    meta = meta[~meta["actual_order"].isna()]
    failcnt = 0
    for i in meta.index:
        name = meta.loc[i]["id"]
        main_name = meta.loc[i]["main_name"]
        version = meta.loc[i]["compiler_version"]
        version = get_ver_from_metastr(version)
        j = build_project(name, main_name, version)
        if j != 0:
            print(name, main_name)
            failcnt += 1
    print(f"fail: {failcnt}")
            
def mk_outdirs():
    meta = pd.read_csv(META_PATH)
    for i in range(len(meta)):
        order = meta["actual_order"].isna()
        if order[i]:
            continue
        name = meta.iloc[i]["id"]
        os.system(f"mkdir output/{name}")
        
def chk_built_well():
    l = os.listdir('for-rlf')
    l.sort()
    for name in l:
        ll = os.listdir(f'for-rlf/{name}')
        if 'transactions.json' not in ll:
            print(name)
        
def reset_outdirs():
    l = os.listdir('output')
    for name in l:
        os.system(f'rm -f output/{name}/*')
        os.system(f'rm -f output/{name}/.stdout.txt')
        os.system(f'rm -f output/{name}/.stderr.txt')
        
def param_to_arg(p: str) -> str | None:
    p = p.strip()
    if p.startswith('string'):
        return '"rlf"'
    elif p.startswith('address'):
        return "address(0x123)"
    elif p.startswith('bytes') and len(p) > 5:
        return "0x456"
    elif p.startswith('uint') or p.startswith('int'):
        return "78"
    elif p.startswith('bool'):
        return "true"
    else:
        # there are no ways to put array-likes in constructors directly
        return None
        
def template(src: str, main_name: str, params: str):
    if params == "": 
        return src
    params_ = params.split(',')
    args = [param_to_arg(param) for param in params_]
    if None in args:
        args_: str = ", ".join([p.split(' ')[-1] for p in params_])
    else:
        params = ""
        args_: str = ", ".join(args)
    main_name_1 = main_name + "_1"
    ret = f"""{src.replace(main_name, main_name_1)}
    
contract {main_name} is {main_name_1} {{
    constructor({params}) {main_name_1}({args_}) public {{ }}
}}
"""
    return ret

def find_main_contract_src(src: str, main_name: str):
    src = "      " + src
    src_l = re.split(r"([^a-zA-Z0-9_]contract[^{}]*{)", src)
    for i in range(len(src_l)):
        if "contract" == src_l[i][1:9] and main_name == src_l[i][9:-1].strip().split(' ')[0]:
            return src_l[i + 1]
    raise ZeroDivisionError
        
def extract_constr_params(src: str, main_name: str):
    constructor_pattern = r'constructor\s*\(([^)]*)\)'
    match = re.search(constructor_pattern, src)
    if match:
        return match.group(1).replace("\n", "").strip()
    else:
        constructor_pattern2 = fr'function\s+{main_name}\s*\(([^)]*)\)'
        match = re.search(constructor_pattern2, src)
        if match:
            return match.group(1).replace("\n", "").strip()
        return ""
    
def getconstructor(src: str, main_name: str):
    src = erase_comment(src)
    src_contract = find_main_contract_src(src, main_name)
    params = extract_constr_params(src_contract, main_name)
    return template(src, main_name, params)
    
def iterate_extract():
    meta = pd.read_csv(META_PATH)
    for i in range(len(meta)):
        order = meta["actual_order"].isna()
        if order[i]:
            continue
        id = meta.iloc[i]["id"]
        main_name = meta.iloc[i]["main_name"]
        with open(f"{CONTRACTS_PATH}/{id}.sol", "r") as f:
            src = f.read()
        getconstructor(src, main_name)

def main():
    build_project_all()
    chk_built_well()
    
if __name__ == "__main__":
    main()
    
    
"""
autobuild failure
0x0846aadb38d580e2870ac0ba6f3625d8cf58cd4e
0x1b2f1ddb9696bb6869999636121277c271f6f23c
0x1f7f65b220a0f2d48b28390380f57468e7a382fe
0x2cecf8f9f328d80d34cd456cc7dd15e20e45638a
0x2f0433758bcc851d047d0fed1f423fa38e96da47
0x331769de477c2122638fe1116808d3a9159ab13a
0x3c0dc66381a4e40d9afbfc33f74e503eb8099f27
0x403e518f21f5ce308085dcf6637758c61f92446a
0x4b89f8996892d137c3de1312d1dd4e4f4ffca171
0x539da201f33a25e4a782d3b42eb0f0a83c0fd753
0x5943a67e4375b556df0856fdf08f972d02677b78
0x5c3a228510d246b78a3765c20221cbf3082b44a4
0x625f220be6440c14f3481072f1cbe9a83a58ec75
0x707784ed2b464474f8fb763c058b33fcf8626db5
0x766ce08cd40b31b79b3681bce55f61e6efc4edfa
0x7e7483db7765408fa66ef9c3f2af7efe365958ed
0x7fea965a502f3f17851a57bcc57e95e03d83e98a
0x809416858a4d0caa83a660c54b59c4180c6d1be3
0x845581267b3ffcde72a1a98efcffee48cb9ae0de
0x8c4ccf23d8674a04665e9e7a64260aa4c0030aeb
0x8c9d4a3b17f0e080ed9f6190181aac7058ced7ac
0x9348995663aadc44f725eaf56d42c9f74ce0162b
0x9e67c652aef503926e0dbaad1535b604ddb72db7
0xa82873dbb0835dca5c273363eeb006342e696036
0xb052d4d50577e2081f85558ae84b47feb11a2fb8
0xb09b1500e55b845966705f0881187f4d3d8b64d8
0xb4c757a71d084df8f9f393cc47639701f595fd87
0xb4fe1fe5f36f239b811be635622d3c5a92ad3cd0
0xb65e87523a06915210b98d440a554b850ee016f7
0xb795be5b5fae8252bb4641b77c670431b1eab601
0xbbd5a76f72e876ff9f9bd4829298855a85c3fa51
0xbe3aad36965853cbabbc9ae6110ced92fc9d51e8
0xc0d0a87b8e18811b863b50494867369588027063
0xc5559887f94ea5c406b3f6d582dc738c4cf0eace
0xce8296f1dacd7c6eca28453f37fe15e0596cdaea
0xd099c363eb969f405099f17b413e2ebdc7018a7d
0xe716209f5d62efa8ca4cacdda1284bfff8df9e42
0xe82062f270dbbd296ec4820845129d874ea35ace
0xe91b8b5e38ad85358d6c5ca6c0092e12bd736b7e
0xeb31c53605b2294fa5476326641d4a78ef2394ab
0xf2e88e0bfe61e5e41d9317e82c6938e67a913cc1
0xf3ba18b53cff2ea3d45e8f06ffe6dc24a32a633e
"""