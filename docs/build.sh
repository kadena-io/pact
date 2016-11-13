#!/bin/bash

# pip install sphinx sphinx-autobuild sphinx_rtd_theme

rm -rf _build

pandoc -s -t rst pact-reference.md pact-functions.md -o pact-reference.rst

# escape +, - headings
perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-reference.rst

sphinx-build -b html -d _build/doctrees . _build/html
