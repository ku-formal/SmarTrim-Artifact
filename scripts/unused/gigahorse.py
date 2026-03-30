"""
prerequisite: docker image of gigahorse.

install via docker (amd64): 
`curl -s -L https://raw.githubusercontent.com/nevillegrech/gigahorse-toolchain/master/scripts/docker/install/install_amd64 | bash`
"""

import argparse
import os
import subprocess
import numpy as np
import pandas as pd

OUTDIR = 'for-smartian/ls.gig'

def get_commands(target_dir: str, df) -> list:
    cmds = []
    for i in df.index:
        id: str = df['id'][i]
        target_hexname = os.path.join(target_dir, id + '.hex')
        cmd = f'''\
        gigahorse \
        -T 600 \
        --client /opt/gigahorse/gigahorse-toolchain/clients/visualizeout.py \
        {target_hexname} && mv .temp/* {OUTDIR} \
        '''
        cmds.append(cmd)
    return cmds


def run_command(cmd) -> tuple[str, subprocess.CompletedProcess]:
    return cmd, subprocess.run(cmd, shell=True)
    

def main():
    # maybe we cannot use multiprocessing for gigahorse docker, idk
    parser = argparse.ArgumentParser()
    parser.add_argument('--target', type=str, default='for-smartian/ls.hex')
    parser.add_argument('--outdir', type=str, default='for-smartian/ls.gig')
    
    args = parser.parse_args()
    print(args)
    
    global OUTDIR
    OUTDIR = args.outdir
    
    os.makedirs(args.outdir, exist_ok=False)
    
    df = pd.read_csv('~/smartrim-benchmark/meta/ls.csv', dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    
    failed_list = []
    
    cmds = get_commands(args.target, df)
    rets = [run_command(cmd) for cmd in cmds]
        
    for cmd, ret in rets:
        if ret.returncode != 0:
            print(f'failed: {" ".join(cmd)}')
            
    os.system("rm -fr .temp")
        
    with open(os.path.join(args.outdir, 'failed.log'), 'w') as fp:
        for filename in failed_list:
            fp.write(filename)
            fp.write('\n')
            
    os.system("rm results.json")
    # os.system("rm -fr ~/PrettySmart/data/smartbugs_tac/*")
    # os.system(f"cp -r {args.outdir}/* ~/PrettySmart/data/smartbugs_tac")
    
    cp()
    
    
def cp():
    df = pd.read_csv('~/smartrim-benchmark/meta/ls.csv', dtype=np.object_)
    df = df[~df['actual_order'].isna()]
    for i in df.index:
        id = df['id'][i]
        if not os.path.exists(f'for-smartian/ls.gig/{id}/out/contract.tac'):
            print(id)
        os.system(f"cp -r for-smartian/ls.gig/{id} ~/PrettySmart/data/smartbugs_tac/{id}")


if __name__ == '__main__':
    cp()