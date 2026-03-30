"""
Deduplicator.
"""


import multiprocessing
import numpy as np
import pandas as pd
import difflib
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
        
        
# def get_ver(s: str) -> str:
#     version_pattern = 'v\d\.\d\.\d{1,2}'
#     version = re.findall(version_pattern, s)
#     return version[0][1:]
    
    
def lines(s: str):
    return len(s.split('\n'))


def normalize_addr(s: str):
    pat = re.compile(r"0x[0-9a-f]+")
    return pat.sub('0x0', s)


def normalizer(s: str):
    s = erase_comment(s)
    s = normalize_addr(s)
    s = s.lower()
    s = s.replace('\n', ' ')
    return s


def initialize_worker(srcs: list[str]):
    setattr(multiprocessing.current_process(), 'srcs', srcs)


def chk(s: tuple[int, int]):
    srcs = getattr(multiprocessing.current_process(), 'srcs', None)
    if type(srcs) != list:
        print(type(srcs))
        exit(1)
    i, j = s
    return difflib.SequenceMatcher(None, srcs[i], srcs[j], True).ratio(), j
    
    
def main(dataset: str):
    df_meta = pd.read_csv(f'meta/{dataset}.csv', dtype=np.object_)
    print(df_meta)
    n = len(df_meta)
    path = f"contracts/{dataset}"
    duplicate_of : list[str | None] = [None for _ in range(n)]
    
    for i in range(n):
        if 'noelsu' in df_meta.keys() and not df_meta['noelsu'].isna()[i]:
            duplicate_of[i] = '<noelsu>'
            continue
        if 'fail' in df_meta.keys() and not df_meta['fail'].isna()[i]:
            duplicate_of[i] = '<failed>'
            continue
        if 'vyper' in df_meta['compiler_version']:
            duplicate_of[i] = '<vyper>'
            continue
        
    indexes = [i for i in range(n) if duplicate_of[i] is None]
    
    # preprocess source codes
    srcs = ["" for _ in range(n)]
    for i in range(n):
        if duplicate_of[i] is not None:
            continue
        name = df_meta["id"][i] + '.sol'
        with open(path + f"/{name}") as f1:
            src = f1.read()
        src = normalizer(src)
        main_name = df_meta["main_name"][i]
        src = src.replace(main_name.lower(), 'SmartrimContract')
        srcs[i] = src
    
    print("preprocessing done, start main work...")
    pool = multiprocessing.Pool(processes=6, initializer=initialize_worker, initargs=(srcs,))
    for i in indexes:
        j_indexes = []
        for j in indexes:
            if i <= j:
                break
            if duplicate_of[j] is not None:
                continue
            if df_meta["address"][i].lower() == df_meta["address"][j].lower() and \
                '_' in df_meta['id'][i] and '_' in df_meta['id'][j]:
                # contract I may be artificially modified from contract J
                continue
            if abs(int(df_meta['loc'][i]) - int(df_meta['loc'][j])) > 20:
                continue
            j_indexes.append(j)
        args = [(i, jj) for jj in j_indexes]
        for s, j in pool.imap(chk, args):
            if s >= 0.985:
                print(df_meta['id'][i], df_meta['id'][j])
                duplicate_of[i] = df_meta["id"][j]
                break
                    
    print(duplicate_of)
    print(len([x for x in duplicate_of if x is None]))
    df_meta['duplicate_of'] = duplicate_of
    df_meta.to_csv(f'meta/{dataset}2.csv', index=False)
    
    
def reorder(dataset: str):
    P = f'meta/{dataset}2.csv'
    df = pd.read_csv(P)
    df = df.sort_values('code_hash')
    print(df)
    actual_order = []
    nodup = df['duplicate_of'].reset_index(drop=True).isna()
    print(nodup)
    cnt = 1
    for i in range(len(df)):
        if nodup[i]:
            actual_order.append(float(cnt))
            cnt += 1
        else:
            actual_order.append(None)
    df['actual_order'] = actual_order
    df = df.sort_index()
    print(df)
    df.to_csv(f'meta/{dataset}2.csv', index=False)

    
if __name__ == '__main__':
    dataset = "io"
    main(dataset)
    reorder(dataset)
    
