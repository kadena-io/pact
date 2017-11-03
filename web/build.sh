#!/bin/sh

chirp() { [ $verbose ] && shout "$*"; return 0; }

shout() { echo "$0: $*" >&2;}

barf() { shout "$*"; exit 111; }

safe() { "$@" || barf "cannot $*"; }

safe stack build --stack-yaml stack-ghcjs.yaml

distdir=.stack-work/dist/x86_64-osx/Cabal-1.24.2.0_ghcjs/build/Pact/pact.jsexe
safe pushd $distdir
safe ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node --externs=all.js.externs > all.min.js
safe popd
safe cp $distdir/all.min.js web/pact.min.js

exit 0
