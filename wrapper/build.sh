#!/bin/bash

chmod +x docker/*.sh
chmod +x wrapper/*.sh

docker build . -f docker/artifact-setter.Dockerfile -t artifact-setter

if [ $? -ne 0 ]; then 
    echo 'docker build artifact-setter: failed'; exit 1
fi

docker volume create smartrim-artifact-solc-select
docker volume create smartrim-artifact-4smartian

docker run --rm --volume smartrim-artifact-solc-select:/root/.solc-select --entrypoint python \
    artifact-setter /root/smartrim-experiment/scripts/install_solc.py
    
docker run --rm --volume smartrim-artifact-solc-select:/root/.solc-select \
    artifact-setter -c 'cp /root/sh-utils/solc-setter.sh /root/.solc-select/solc-setter.sh'
    
docker run --rm \
    --volume $HOME/smartrim-benchmark:/root/smartrim-benchmark \
    --volume smartrim-artifact-solc-select:/root/.solc-select \
    --volume smartrim-artifact-4smartian:/root/smartrim-experiment/for-smartian \
    --workdir /root/smartrim-experiment \
    artifact-setter \
    -c "python /root/smartrim-experiment/scripts/gen_abi_bin2.py --dataset io && \
        python /root/smartrim-experiment/scripts/gen_abi_bin2.py --dataset ls && \
        python /root/smartrim-experiment/scripts/gen_abi_bin2.py --dataset re \
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