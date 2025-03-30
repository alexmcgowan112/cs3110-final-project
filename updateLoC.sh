#!/bin/bash

dune clean

# Get lines of code using `cloc --by-file --include-lang=OCaml .`
LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

# Print updated lines of code
echo "The project now has $LOC lines of code."

# Update the first line of README.md
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS: Requires an empty string after -i
    sed -i '' "1s|.*|### Lines of Code: $LOC/1600|" README.md
else
    # Linux: Standard GNU sed
    sed -i "1s|.*|### Lines of Code: $LOC/1600|" README.md
fi

dune build
