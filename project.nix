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
          ver = "2.3.27";
          sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
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

        mod = self.callHackageDirect {
          pkg = "mod";
          ver = "0.1.2.2";
          sha256 = "09x2i8crbpyvfzs478r5xky4vrfg6xjv11g3vaj3kb16pbd1raqn";
        } {};

        poly = dontCheck (self.callHackageDirect {
          pkg = "poly";
          ver = "0.5.1.0";
          sha256 = "1c65qcgvvbx3hlfgd5pzx2g5aam806w89x9nl7nhwx069mnixvn1";
        } {});

        quickcheck-classes = self.callHackageDirect {
          pkg = "quickcheck-classes";
          ver = "0.6.5.0";
          sha256 = "12sksx6sxv6h0bnd5q71d5fm6f6mf40vigncg04sqqry90x5gfdg";
        } {};

        quickcheck-classes-base = self.callHackageDirect {
          pkg = "quickcheck-classes-base";
          ver = "0.6.2.0";
          sha256 = "1kzw9kx9ab1apdyka8r63pkd35jpmdb21ds4x0s0hlyv2rka3f8w";
        } {};

        semirings = self.callHackageDirect {
          pkg = "semirings";
          ver = "0.6";
          sha256 = "0nzj3fpdyvmx3z6hd1zbxyfmz2w6f95s01yr5cwgq0bp86vqpnfa";
        } {};

        vector-algorithms = self.callHackageDirect {
          pkg = "vector-algorithms";
          ver = "0.8.0.4";
          sha256 = "15zwgdi80ihy6cypqm3g24418kshlydxjk5c2dvdd534wylyw8pp";
        } {};

        vector-sized = self.callHackageDirect {
          pkg = "vector-sized";
          ver = "1.4.2";
          sha256 = "0ijazq59892zpjjfxqa8c6jmv8llrjdr9ypjwz5yrkhvxc8mqld3";
        } {};

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
