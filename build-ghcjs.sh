BUILDROOT="dist-ghcjs"
OUTDIR=$BUILDROOT/build/x86_64-osx/ghcjs-0.2.1/pact-ghcjs-2.3.8/c/pact/build/pact/pact.jsexe
cabal --project-file=cabal-ghcjs.project --builddir=$BUILDROOT new-build pact-ghcjs
cp $OUTDIR/* pact-ghcjs/site
