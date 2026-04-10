#!/bin/bash

execute() {
    local arg1="$1"
    local arg2="$2"
    
    uv run scripts/execute.py --dataset "$arg1" --tool "$arg2"

    local exit_code=$?
    
    if [ $exit_code -eq 0 ] || [ $exit_code -eq 88 ]; then
        return 0
    else
        exit 1
    fi
} 

execute io smartrim
execute ls smartrim
execute re smartrim
