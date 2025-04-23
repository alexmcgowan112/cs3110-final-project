#!/bin/bash

dune clean

LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

echo "The project now has $LOC lines of code."

# Update the first line of README.md with lines of code
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS: Requires an empty string after -i
    sed -i '' "1s|.*|### Lines of Code: $LOC/1600|" README.md
else
    # Linux: Standard GNU sed
    sed -i "1s|.*|### Lines of Code: $LOC/1600|" README.md
fi

# Run tests with bisect_ppx and generate the coverage report
dune test --instrument-with bisect_ppx --force && bisect-ppx-report html && open _coverage/index.html

# Extract the coverage percentage from the generated index.html
COVERAGE=$(perl -nle 'print $1 if /<meta name="description" content="([0-9.]+)% coverage overall"/' _coverage/index.html)

echo "The project now has $COVERAGE% test coverage."

# Update the second line of README.md with the coverage percentage
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS: Requires an empty string after -i
    sed -i '' "3s|.*|### Coverage: $COVERAGE%/80%|" README.md
else
    # Linux: Standard GNU sed
    sed -i "3s|.*|### Coverage: $COVERAGE%/80%|" README.md
fi

dune build