from solcx import get_installable_solc_versions, install_solc
import os

def install_all_solc_versions():
    versions = get_installable_solc_versions()
    print(f"Available solcs: {versions}")

    for version in versions:
        try:
            print(f"Installing solc version {version}...")
            install_solc(version)
            print(f"Successfully installed solc version {version}")
        except Exception as e:
            print(f"Failed to install solc version {version}: {e}")
            
def mk_dynlinks():
    os.mkdir('/root/.solcx/solcs')
    versions = get_installable_solc_versions()
    
    for version in versions:
        os.system(f'mkdir /root/.solcx/solcs/{version}')
        os.system(f'ln -s /root/.solcx/solc-v{version} /root/.solcx/solcs/{version}/solc')

if __name__ == "__main__":
    install_all_solc_versions()
    mk_dynlinks()