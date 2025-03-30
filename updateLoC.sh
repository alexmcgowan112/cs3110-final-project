#!/bin/bash

dune clean

# Get lines of code
LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

# Print updated lines of code
echo "The project now has $LOC lines of code."

# Update the first line of README.md
sed -i '' "1s|.*|### Lines of Code: $LOC/1600|" README.md

dune build
