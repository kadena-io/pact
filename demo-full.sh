#!/bin/sh

echo "# ------------------------------------------------------ #"
echo "# source file: examples/analyze-tests/analyze-tests.pact #"
echo "# ------------------------------------------------------ #"
cat examples/analyze-tests/analyze-tests.pact

stack exec -- pact-analyze-exe
