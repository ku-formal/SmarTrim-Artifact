#!/bin/bash

docker volume rm smartrim-artifact-solc-select || echo "Cannot remove 'smartrim-artifact-solc-select'."
docker volume rm smartrim-artifact-4smartian || echo "Cannot remove 'smartrim-artifact-4smartian'."

IMAGES_TO_REMOVE=(
    "artifact-setter:fse26"
    "my-smartrim:fse26"
    "my-achecker:fse26"
    "my-lent:fse26"
    "my-smartest:fse26"
    "my-smartian:fse26"
    "my-slither:fse26"
)

for image in "${IMAGES_TO_REMOVE[@]}"; do
    docker rmi -f "$image" 2>/dev/null || echo "Image '$image' not found."
done

PULLED_IMAGES=(
    "christoftorres/confuzzius"
    "ghcr.io/uni-due-syssec/efcf-framework"
    "mythril/myth"
    "damonhero/rlf:latest"
    "holmessherlock/sailfish:latest"
    "janson1060/slise_tool:v1"
)

for image in "${PULLED_IMAGES[@]}"; do
    docker rmi -f "$image" 2>/dev/null || echo "Image '$image' not found."
done
