#!/bin/sh

echo "# ------------------------------------------------------ #"
echo "# source file: examples/analyze-tests/analyze-tests.pact #"
echo "# ------------------------------------------------------ #"
echo ""
cat examples/analyze-tests/analyze-tests.pact
echo ""
echo "# ----------------------------------- #"
echo "# compiled SMT for \"analyze-tests.pay\" #"
echo "# ----------------------------------- #"
echo ""
stack exec -- pact-analyze-exe
