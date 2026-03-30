import pandas as pd
import json
import os

import argparse


CVE_TRAIN_DIR = 'expr_result/cve_30_120b_0112/p'


def get_metafile(dataset: str) -> str:
    if dataset == 'cve':
        meta = "../Benchmark/meta/cve.csv"
    elif dataset == 'ls':
        meta = "../Benchmark/meta/leaking-suicidal.csv"
    else:
        raise ValueError("")
    
    return meta


def get_traindir(dataset: str) -> str:
    if dataset == 'cve':
        return CVE_TRAIN_DIR
    else:
        raise ValueError("")


def get_targets(dataset: str, fold_id: int) -> set[str]:
    meta = get_metafile(dataset)
    df = pd.read_csv(meta)
    df = df.sort_values('code_hash')
    filenames = df['id']
    new_filenames = []
    for i in range(len(filenames)):
        if i % 4 != fold_id % 4:
            new_filenames.append(f'{filenames[i]}.json')
            
    return set(new_filenames)


def read(json_path: str) -> pd.DataFrame:
    """
    read a json file and return a dataframe of vuls
    """
    try:
        with open(json_path, 'r') as f:
            j = json.load(f)
    except FileNotFoundError:
        return pd.DataFrame(data=None)
    fid = j['baseName'].replace('.sol', '')
    if 'result' not in j:
        return pd.DataFrame(data=None)
    df = {
        'fid': [],
        'qid': [],
        'kind': [],
        'depth': [],
        'disproven_time': [],
        'raw': [],
        'raw2': [],
    }
    df = pd.DataFrame(df)
    l = j['result']
    for dic in l:
        qid = f"{fid}_{dic['no']:0>2}"
        kind = dic['kind']
        depth = dic['depth']
        disproven_time = dic['disprovenTime']
        raw = dic['raw']
        raw2 = dic['raw2']
        df.loc[len(df)] = (fid, qid, kind, depth, disproven_time, raw, raw2)
    return df


def read_on(dataset: str, fold_id: int):
    files = get_targets(dataset, fold_id)
    dir_path = get_traindir(dataset)
    df = pd.DataFrame()
    for f in files:
        df2 = read(os.path.join(dir_path, f))
        df = pd.concat([df, df2])
    df = df.sort_values(by='disproven_time', key=lambda x: x.map(float))
    df = df.reset_index(drop=True)
    df.to_csv('src/exploit/train/model.csv', index=False)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--dataset', type=str, default='cve', choices=['cve', 'ls'], help='dataset')
    args = parser.parse_args()
    
    for i in range(1, 5):
        os.makedirs(f'./_model/_{i}', exist_ok=True)
    
    for i in range(1, 5):
        read_on(args.dataset, i)
    
        os.system(f'python3 src/exploit/train/train.py --ngram 3 --input src/exploit/train/model.csv -o ./_model/_{i}')
    

if __name__ == '__main__':
    main()