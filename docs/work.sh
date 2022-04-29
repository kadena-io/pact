#!/bin/bash

# --- English Docs --- #
cd en/
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

# --- Japanese Docs --- #
# cd ..
# cd ja/
# rm -rf _build

# pandoc -s -t rst pact-reference.md -o pact-reference.rst
# perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-reference.rst
# sphinx-build -b html -d _build/doctrees . _build/html

# --- Korean Docs --- #
# cd ..
# cd ko/
# rm -rf _build

# pandoc -s -t rst pact-reference.md -o pact-reference.rst
# perl -p0777i -e 's/^(\+|\-)\n~/\\\1\n~~/gm' pact-reference.rst
# sphinx-build -b html -d _build/doctrees . _build/html
