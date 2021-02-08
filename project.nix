{ rpRef ? "dfac4599b37bbfdb754afa32d25ba4832623277a"
, rpSha ? "03hicg0x77nz4wmwaxnlwf9y0xbypjjdzg3hak756m1qq8vpgc17"
, system ? builtins.currentSystem
}:

let kpkgs = import ./dep/kpkgs { inherit system; };
in {
  inherit kpkgs;
  proj =
    kpkgs.rp.project ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
      name = "pact";
      overrides = self: super: {
        # The z3 dependency needs to be conditional so pact can be a
        # dependency of ghcjs apps.
        pact = if self.ghc.isGhcjs or false
                 then super.pact
                 else addBuildDepend super.pact pkgs.z3;
      };
      packages = {
        pact = kpkgs.gitignoreSource ./.;

        # Example of how to override kpkgs using callHackageDirect
        # foo = dontCheck (kpkgs.callHackageDirect {
        #   pkg = "foo";
        #   ver = "0.1";
        #   sha256 = "0000000000000000000000000000000000000000000000000000";
        # } {});

      };
      shellToolOverrides = ghc: super: {
        z3 = pkgs.z3;
        stack = pkgs.stack;
      };
      shells = {
        ghc = ["pact"];
        ghcjs = ["pact"];
      };
    });
}
