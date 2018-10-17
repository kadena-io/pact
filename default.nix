{ rpRef ? "92f235e0533b9722be729d785a033faf21e81872", rpSha ?  "1r3kcvy0zf6qdvnasins43hb77lpn0r26vvmnh3cf3yrvpy86sam" }:

let rp = (import <nixpkgs> {}).fetchFromGitHub {
           owner = "reflex-frp";
           repo = "reflex-platform";
           rev = rpRef;
           sha256 = rpSha;
         };

in
  (import rp {}).project ({ pkgs, ... }: {
    name = "pact";
    withHoogle = false;
    overrides = self: super:
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
       in {
            pact = pkgs.haskell.lib.addBuildDepend super.pact pkgs.z3;
            aeson = self.callHackage "aeson" "1.4.0.0" {};
            haskeline = guardGhcjs super.haskeline;

            # Needed to get around a requirement on `hspec-discover`.
            megaparsec = pkgs.haskell.lib.dontCheck super.megaparsec;

            criterion = self.callHackage "criterion" "1.4.0.0" {};

            hedgehog = self.callCabal2nix "hedgehog" (pkgs.fetchFromGitHub {
              owner = "hedgehogqa";
              repo = "haskell-hedgehog";
              rev = "38146de29c97c867cff52fb36367ff9a65306d76";
              sha256 = "1z8d3hqrgld1z1fvjd0czksga9lma83caa2fycw0a5gfbs8sh4zh";
            } + "/hedgehog") {};

            # sbv >= 7.9
            sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "sbv";
              rev = "3dc60340634c82f39f6c5dca2b3859d10925cfdf";
              sha256 = "18xcxg1h19zx6gdzk3dfs87447k3xjqn40raghjz53bg5k8cdc31";
            }) {});

            # Our own custom fork
            thyme = pkgs.haskell.lib.dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {});

            weeder = self.callHackage "weeder" "1.0.5" {};
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
      ghc-mod = null;
    };
    shells = {
      ghc8_0 = ["pact"];
      # ghcjs8_0 = ["pact"];
    };

  })
