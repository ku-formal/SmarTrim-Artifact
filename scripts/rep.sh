#!/bin/bash

usage() {
    echo "Usage: $0 <dir> <target> <replace>"
    echo ""
    echo "Example:"
    echo "  $0 ./data oldword newword"
    echo "  $0 /path/to/json/files \"old text\" \"new text\""
    exit 1
}

if [[ $# -ne 3 ]]; then
    usage
fi

DIRECTORY="$1"
OLD_WORD="$2"
NEW_WORD="$3"

if [[ ! -d "$DIRECTORY" ]]; then
    echo "Error: '$DIRECTORY' does not exist"
    exit 1
fi

TOTAL_FILES=0
CHANGED_FILES=0

while IFS= read -r -d '' file; do
    ((TOTAL_FILES++))
    
    if grep -q "$OLD_WORD" "$file"; then
        ((CHANGED_FILES++))
        echo "Processing: $file"
        
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            sed -i '' "s/$OLD_WORD/$NEW_WORD/g" "$file"
        else
            # Linux
            sed -i "s/$OLD_WORD/$NEW_WORD/g" "$file"
        fi
        
        echo "  → Changed"
    fi
    
done < <(find "$DIRECTORY" -name "*.json" -type f -print0)

echo "----------------------------------------"
echo "Total files: $TOTAL_FILES"
echo "Changed files: $CHANGED_FILES"