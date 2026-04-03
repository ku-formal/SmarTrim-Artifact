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

execute io smartrimbase
execute io smartrimr
execute io smartrimbaser

execute ls smartrimbase
execute ls smartrimr
execute ls smartrimbaser

execute re smartrimbase
execute re smartrimr
execute re smartrimbaser
