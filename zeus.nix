{}:
let 
  proj = import ./. {} ;
in 
{
  pact = proj.ghc.pact;
  pact-ghcjs = proj.ghcjs.pact;
}
