import json
import numpy as np
import os
import pandas as pd

HOME = os.path.expanduser('~')
JSON_DIR = f'{HOME}/smartrim-experiment/result/ls.prettysmart.json'
META = f'{HOME}/smartrim-benchmark/meta/ls.csv'

def main():
    df = pd.read_csv(f'{HOME}/smartrim-benchmark/data-source/prettysmart/res.csv', dtype=np.object_)
    df = df[df['label'] == 'T']
    df_meta = pd.read_csv(META, dtype=np.object_)
    json_names = os.listdir(JSON_DIR)
    ids = [s.replace('.json', '') for s in json_names]
    df_right = pd.DataFrame({'id': ids})
    df_right['tested'] = True
    
    df = df.merge(df_right, on='id', how='left')
    df_not_tested = df[df['tested'].isna()]
    df_not_tested = df_not_tested.merge(df_meta, how='left', on='id')
    df_not_tested.to_csv(f'{HOME}/smartrim-experiment/misc/not-tested.csv', index=False)
    
    df = df[~df['tested'].isna()]
    print(df)
    
    prd_find_false = 0
    no_pc_bypassed_count = 0
    total_false = 0
    for i in df.index:
        row = df.loc[i]
        id = row['id']
        with open(f'{JSON_DIR}/{id}.json', 'r') as fp:
            j = json.load(fp)
        if len(j['PC_bypassed']) == 0:
            no_pc_bypassed_count += 1
        if j['PRD_find'] == False:
            prd_find_false += 1
        if len(j['PC_bypassed']) == 0 and j['PRD_find'] == False:
            total_false += 1
    print(len(df))
    print(no_pc_bypassed_count)
    print(prd_find_false)
    print(total_false)

if __name__ == '__main__':
    main()