{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc", rpSha ?  "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j" }:

let rp = builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
  sha256 = rpSha;
};
overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "727929c9af003d71eab1f0d90cc8e01761943491";
      sha256 = "02p8rhflimc852ysgf7nmaypz6ga3w4iss3z8d3qrby5a2d464p9";
    };
  });
};

in
  (import rp { nixpkgsOverlays = [ overlay ]; }).project ({ pkgs, ... }: {
    name = "pact";
    overrides = self: super:
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
       in {
            pact = pkgs.haskell.lib.addBuildDepend super.pact pkgs.z3;
            haskeline = guardGhcjs super.haskeline;

            # tests for extra were failing due to an import clash (`isWindows`)
            extra = pkgs.haskell.lib.dontCheck super.extra;
            # tests try to use ghc-pkg and cabal (https://github.com/sol/doctest/issues/213)
            doctest = guardGhcjs (pkgs.haskell.lib.dontCheck (self.callHackage "doctest" "0.16.0" {}));
            # these want to use doctest, which doesn't work on ghcjs
            bytes = pkgs.haskell.lib.dontCheck super.bytes;
            intervals = pkgs.haskell.lib.dontCheck super.intervals;
            bound = pkgs.haskell.lib.dontCheck super.bound;
            trifecta = pkgs.haskell.lib.dontCheck super.trifecta;
            lens-aeson = pkgs.haskell.lib.dontCheck super.lens-aeson;
            # test suite for this is failing on ghcjs:
            hw-hspec-hedgehog = pkgs.haskell.lib.dontCheck super.hw-hspec-hedgehog;

            algebraic-graphs = pkgs.haskell.lib.dontCheck super.algebraic-graphs;

            # Needed to get around a requirement on `hspec-discover`.
            megaparsec = pkgs.haskell.lib.dontCheck super.megaparsec;

            hedgehog = self.callCabal2nix "hedgehog" (pkgs.fetchFromGitHub {
              owner = "hedgehogqa";
              repo = "haskell-hedgehog";
              rev = "38146de29c97c867cff52fb36367ff9a65306d76";
              sha256 = "1z8d3hqrgld1z1fvjd0czksga9lma83caa2fycw0a5gfbs8sh4zh";
            } + "/hedgehog") {};
            hlint = self.callHackage "hlint" "2.0.14" {};
            # hoogle = self.callHackage "hoogle" "5.0.15" {};

            # type-level-sets 0.8.9.0
            type-level-sets = pkgs.haskell.lib.dontCheck (self.callCabal2nix "type-level-sets" (pkgs.fetchFromGitHub {
              owner = "dorchard";
              repo = "type-level-sets";
              rev = "3c4d979354716993f1502eca82df1c9a5854441d";
              sha256 = "0jh8lq3h37xhbgparf1giii8lph3vyd7p59qhdp9hgfkj0qsdx93";
            }) {});

            # sbv with a patch to disable "unsupported query call" until it's fixed upstream
            sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "joelburget";
              repo = "sbv";
              rev = "cb367f213d6a9c5acbcd6873776e571dcda2e45d";
              sha256 = "1jvi8x4z4phx9yrmgq0ymz938fnrzjc8nzirp470ndi1xkhbqkn1";
            }) {});

            # need crackNum 2.3
            crackNum = pkgs.haskell.lib.dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "crackNum";
              rev = "54cf70861a921062db762b3c50e933e73446c3b2";
              sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
            }) {});

            # Our own custom fork
            thyme = pkgs.haskell.lib.dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {});

            # weeder = self.callHackage "weeder" "1.0.5" {};
            weeder = self.callCabal2nix "weeder" (pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "weeder";
              rev = "56b46fe97782e86198f31c574ac73c8c966fee05";
              sha256 = "005ks2xjkbypq318jd0s4896b9wa5qg3jf8a1qgd4imb4fkm3yh7";
            }) {};
          };
    packages = {
      pact = builtins.filterSource
        (path: type: !(builtins.elem (baseNameOf path)
           ["result" "dist" "dist-ghcjs" ".git" ".stack-work"]))
        ./.;
    };
    shellToolOverrides = ghc: super: {
      z3 = pkgs.z3;
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["pact"];
      ghcjs = ["pact"];
    };

  })
