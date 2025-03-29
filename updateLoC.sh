#!/bin/bash

dune clean

# Get the lines of code count
LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

# Update the first line of README.md
sed -i '' "1s|.*|### Lines of Code: $LOC/1600|" README.md

# Print the updated number of lines of code
echo "The project now has $LOC lines of code."

dune build
