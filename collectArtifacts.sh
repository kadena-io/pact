#! /bin/sh

# Creates artifacts to be captured by GitLab CI

if [ ! -d result ] ; then nix-build ; fi
if [ ! -d result-doc ] ; then nix-build -A ghc.pact.doc ; fi

pubdir="public-$1"
binary="pact-$1"

rm -fr $pubdir
mkdir -p $pubdir
cp -LR result/ghc/pact/share/hpc/vanilla/html $pubdir/code-coverage
mkdir -p $pubdir/docs
cp -LR `find result-doc/share -name html`/* $pubdir/docs
mkdir -p $pubdir/binaries
cp -LR result/ghc/pact/bin/pact $pubdir/binaries/$binary
chmod -R u+w $pubdir
