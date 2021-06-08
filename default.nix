let 
  pactProj = import ./project.nix {};
  inherit (pactProj) kpkgs proj;
  z3 = proj.passthru.z3;
  pactExe = proj.ghc.pact;
in kpkgs.pkgs.writeScriptBin "pact" ''
  #!/usr/bin/env bash
  export PATH=${z3}/bin:$PATH
  exec ${pactExe}/bin/pact "${"$" + "{@:2}" }"
''
