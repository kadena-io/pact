#!/bin/bash

# pip install sphinx sphinx-autobuild sphinx_rtd_theme

rm -rf _build

pandoc -s -t rst pact-reference.md -o pact-reference.rst
pandoc -s -t rst pact-functions.md -o pact-functions.rst
pandoc -s -t rst pact-properties.md -o pact-properties.rst
pandoc -s -t rst pact-properties-api.md -o pact-properties-api.rst

# escape +, - headings
perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-reference.rst
perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-functions.rst
perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-properties-api.rst

sphinx-build -b html -d _build/doctrees . _build/html
