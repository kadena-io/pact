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

        # sbv requires this even though it is not used in the build (and the hash is invalid)
        tasty-bench = dontCheck (self.callHackageDirect {
          pkg = "tasty-bench";
          ver = "0.3.1";
          sha256 = "0000000000000000000000000000000000000000000000000000";
        } {});

        libBF = doJailbreak (dontCheck (self.callHackageDirect {
          pkg = "libBF";
          ver = "0.6.3";
          sha256 = "0j0i39jb389rnrkkw2xqz10471afxys79nf31hhlqr4fk6ddhjf7";
        } {}));

        sbv = dontCheck (self.callHackageDirect {
          pkg = "sbv";
          ver = "9.0";
          sha256 = "14g2qax1vc7q4g78fa562dviqvcd0l52kd5jmgv90g3g3ci15bnl";
        } {});

        pact-do-benchmark = overrideCabal (doBenchmark (dontCheck super.pact)) (oldDrv: {
          benchmarkSystemDepends = [ pkgs.z3 ];
          postBuild = ''
            mkdir -p log  # needed or else bench can't create a sqlitedb at log/sqlite.db
            ./Setup bench
          '';
          doHaddock = false;
          doCheck = false;
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
