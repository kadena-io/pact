#! /bin/sh

# Creates artifacts to be captured by GitLab CI

if [ ! -d result ] ; then nix-build ; fi
if [ ! -d result-doc ] ; then nix-build -A ghc.pact.doc ; fi

rm -fr public
mkdir -p public
cp -LR result/ghc/pact/share/hpc/vanilla/html public/code-coverage
mkdir -p public/docs
cp -LR `find result-doc/share -name html`/* public/docs
mkdir -p public/binaries
cp -LR result/ghc/pact/bin/pact public/binaries/pact-macos
chmod -R u+w public
