#!/bin/bash

function cpd () {
  mkdir -p "$(dirname "$2")" && cp "$1" "$2"
}

if [[ -z $ARTIFACT_SOLC_VERSION ]]; then
    echo "env <ARTIFACT_SOLC_VERSION> not set"
    exit 75
fi

if [[ -z $ARTIFACT_SOLC_DEST_LOCATION ]]; then
    echo "env <ARTIFACT_SOLC_DEST_LOCATION> not set"
    exit 75
fi

cpd $HOME/.solc-select/artifacts/solc-$ARTIFACT_SOLC_VERSION/solc-$ARTIFACT_SOLC_VERSION \
   $ARTIFACT_SOLC_DEST_LOCATION
   
if [[ $? -ne 0 ]]; then
    echo "solc-setter: cp: failed"
    exit 75
fi

if [[ $# -gt 0 ]]; then
    exec "$@"
else
    echo "interactive shell activated after solc-setter.sh"
    exec /bin/bash
fi 