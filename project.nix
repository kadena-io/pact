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
        hedgehog = doJailbreak super.hedgehog;
        tasty-hedgehog = doJailbreak super.tasty-hedgehog;
        retry = doJailbreak super.retry;
        bifunctors = doJailbreak super.bifunctors;
        http-media = doJailbreak super.http-media;
        semigroupoids = doJailbreak super.semigroupoids;
        dec = doJailbreak super.dec;
        aeson = doJailbreak super.aeson;
        attoparsec = doJailbreak super.attoparsec;
        attoparsec-iso8601 = doJailbreak super.attoparsec-iso8601;
        singleton-bool = doJailbreak super.singleton-bool;
        bool = doJailbreak super.bool;
        quickcheck-instances = doJailbreak super.quickcheck-instances;
        trifecta = doJailbreak super.trifecta;
        http-api-data = doJailbreak super.http-api-data;
        servant = doJailbreak super.servant;
        servant-client = doJailbreak super.servant-client;
        servant-client-core = doJailbreak super.servant-client-core;
        servant-server = doJailbreak super.servant-server;
        wai-extra = doJailbreak super.wai-extra;
        wai-app-static = doJailbreak (self.callHackageDirect {
          pkg = "wai-app-static";
          ver = "3.1.7.4";
          sha256 = "1vbblkgdna1cli6nr05fmd2p47xnrnnkbqdb9zxlzxdshxkblsry";
        } {});
        these = doJailbreak (self.callHackageDirect {
          pkg = "these";
          ver = "1.1.1.1";
          sha256 = "1i1nfh41vflvqxi8w8n2s35ymx2z9119dg5zmd2r23ya7vwvaka1";
        } {});
        http2 = doJailbreak (self.callHackageDirect {
          pkg = "http2";
          ver = "2.0.6";
          sha256 = "05f9hrgs0v3h99pqv948xaysvndxccb5m6mm2mayn0yv8djryhqi";
        } {});
        time-compat = doJailbreak (self.callHackageDirect {
          pkg = "time-compat";
          ver = "1.9.6.1";
          sha256 = "0ika8xx9zff8rwaabs17q5c30c1b9ii89jhbvahi5nk7rs0cd5fs";
        } {});

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
