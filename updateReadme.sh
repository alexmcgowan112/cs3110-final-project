#!/bin/bash

dune clean

# Count the lines of OCaml code in the project using cloc and extract the total.
LOC=$(cloc --by-file --include-lang=OCaml . | awk '/SUM:/ {print $NF}')

echo "The project now has $LOC lines of code."

# Replace the "Lines of Code" line in README.md with the updated count.
perl -pi -e 's|^### Lines of Code:.*|### Lines of Code: '"$LOC"'/1600|' README.md

# Run tests with bisect and generate the coverage report
dune test --instrument-with bisect_ppx --force && bisect-ppx-report html
if [[ "$1" != "--no-gui" && "$1" != "-n" ]]; then
    open _coverage/index.html
fi

# Extract the coverage percentage from the generated index.html
COVERAGE=$(awk -F'[<>]' '/<h2>[0-9.]+%<\/h2>/ {print $3}' _coverage/index.html)

echo "The project now has $COVERAGE test coverage."

# Replace the "Coverage" line in README.md with the updated percentage.
perl -pi -e 's|^### Coverage:.*|### Coverage: '"$COVERAGE"'/80%|' README.md

dune build