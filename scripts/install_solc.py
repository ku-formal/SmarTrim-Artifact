"""
Installs solcs from solc-select. It prevents re-installation.
"""

import os
from utils import solv_lst, solc_select_path

def main():
    for v in solv_lst:
        path = solc_select_path(v, raise_if_not_installed=False)
        if not os.path.exists(path):
            os.system(f'solc-select install {v}')

if __name__ == '__main__':
    main()