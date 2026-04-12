#!/bin/bash

chmod +x docker/*.sh
chmod +x wrapper/*.sh

# install uv
if ! command -v uv &> /dev/null
then
    echo "--- uv is not installed. Start installation. ---"
    curl -LsSf https://astral.sh/uv/install.sh | sh
    export PATH="$HOME/.local/bin:$PATH"
    echo "--- uv installation is done. ---"
else
    uv --version
fi
# install uv end

docker build . -f docker/artifact-setter.Dockerfile -t artifact-setter:fse26

if [ $? -ne 0 ]; then 
    echo 'docker build artifact-setter: failed'; exit 1
fi

docker volume create smartrim-artifact-solc-select
docker volume create smartrim-artifact-4smartian

docker run --rm --volume smartrim-artifact-solc-select:/root/.solc-select --init --entrypoint python \
    artifact-setter:fse26 /root/smartrim-experiment/scripts/install_solc.py
    
docker run --rm --volume smartrim-artifact-solc-select:/root/.solc-select \
    artifact-setter:fse26 -c 'cp /root/sh-utils/solc-setter.sh /root/.solc-select/solc-setter.sh'
    
docker run --rm \
    --volume $(pwd):/root/SmarTrim-Artifact \
    --volume smartrim-artifact-solc-select:/root/.solc-select \
    --volume smartrim-artifact-4smartian:/root/SmarTrim-Artifact/for-smartian \
    --workdir /root/SmarTrim-Artifact \
    artifact-setter:fse26 \
    -c "python /root/SmarTrim-Artifact/scripts/gen_abi_bin2.py --dataset io && \
        python /root/SmarTrim-Artifact/scripts/gen_abi_bin2.py --dataset ls && \
        python /root/SmarTrim-Artifact/scripts/gen_abi_bin2.py --dataset re \
        "
        
if [ $? -ne 0 ]; then 
   echo 'smartrim benchmark: compile failed'
   exit 1
fi

docker build ./SmarTrim -t my-smartrim:fse26

docker pull christoftorres/confuzzius
docker pull ghcr.io/uni-due-syssec/efcf-framework
docker pull mythril/myth
docker pull damonhero/rlf:latest
docker pull holmessherlock/sailfish:latest
docker pull janson1060/slise_tool:v1

docker build . -f docker/achecker.Dockerfile -t my-achecker:fse26
docker build . -f docker/lent-sse.Dockerfile -t my-lent:fse26
docker build . -f docker/smartest.Dockerfile -t my-smartest:fse26
docker build . -f docker/smartian.Dockerfile -t my-smartian:fse26
docker build . -f docker/slither.Dockerfile -t my-slither:fse26
