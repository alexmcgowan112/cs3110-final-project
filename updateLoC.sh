#!/bin/bash

# Step 1: Clean the build
dune clean

# Step 2: Get the lines of code count
LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

# Step 3: Update the first line of README.md
sed -i '' "1s|.*|### Lines of Code: $LOC/1600|" README.md

# Step 4: Build the project
dune build
