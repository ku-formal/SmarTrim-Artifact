#!/bin/bash

execute() {
    local arg1="$1"
    local arg2="$2"
    
    python3 scripts/execute.py --dataset "$arg1" --tool "$arg2"
    local exit_code=$?
    
    if [ $exit_code -eq 0 ] || [ $exit_code -eq 88 ]; then
        return 0
    else
        exit 1
    fi
} 

execute io smartrim
execute io smartrimbase
execute io confuzzius
execute io lent
execute io mythril
execute io smartest
execute io smartian

execute ls smartrim
execute ls smartrimbase
execute ls achecker
execute ls confuzzius
execute ls efcf
execute ls lent
execute ls mythril
execute ls rlf
execute ls slither
execute ls smartest
execute ls smartian

execute re smartrim
execute re smartrimbase
execute re confuzzius
execute re efcf
execute re lent
execute re mythril
execute re sailfish
execute re slise
execute re slither
execure re smartian

execute io smartrimr
execute io smartrimbaser

execute ls smartrimr
execute ls smartrimbaser

execute re smartrimr
execute re smartrimbaser
