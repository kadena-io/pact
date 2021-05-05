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

        pact-time = dontCheck (self.callCabal2nix "pact-time" (pkgs.fetchFromGitHub {
          owner = "kadena-io";
          repo = "pact-time";
          rev = "4fe4b898f1d41823a30946a7d9c870b518ea4dc1";
          sha256 = "1yy1v8b96lwfj1rhgwrlw9i5dx1jd1wna4hcdx1qismiwcwvb8xb";
        }) {});

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
