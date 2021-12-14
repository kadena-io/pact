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
        direct-sqlite = dontCheck (self.callHackageDirect {
          pkg = "direct-sqlite";
          ver = "2.3.26";
          sha256 = "1kdkisj534nv5r0m3gmxy2iqgsn6y1dd881x77a87ynkx1glxfva";
        } {});

        # The z3 dependency needs to be conditional so pact can be a
        # dependency of ghcjs apps.
        pact = dontCheck (if self.ghc.isGhcjs or false
                 then super.pact
                 else addBuildDepend super.pact pkgs.z3);

        pact-time = dontCheck (self.callHackageDirect {
          pkg = "pact-time";
          ver = "0.2.0.0";
          sha256 = "1cfn74j6dr4279bil9k0n1wff074sdlz6g1haqyyy38wm5mdd7mr";
        } {});

        pact-do-benchmark = overrideCabal (doBenchmark (dontCheck super.pact)) (oldDrv: {
          benchmarkSystemDepends = [ pkgs.z3 ];
          postBuild = ''
            mkdir -p log  # needed or else bench can't create a sqlitedb at log/sqlite.db
            ./Setup bench
          '';
          doHaddock = false;
          doCheck = true;
        });
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

      passthru = { inherit (pkgs) z3;};
    });
}
