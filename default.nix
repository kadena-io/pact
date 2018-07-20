{ rpRef ? "80236ad3769602813d1c963e2bd90edd3147734b", rpSha ?  "13l46z12i1bdwl9w76vl0cw860syvjkm8a4zgc0610f98h18dqh1" }:

let rp = (import <nixpkgs> {}).fetchFromGitHub {
           # This repo gives us toolOverrides
           owner = "adetokunbo";
           repo = "reflex-platform";
           rev = rpRef;
           sha256 = rpSha;
         };

in
  (import rp {}).project ({ pkgs, ... }: {
    name = "pact";
    overrides = self: super:
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
       in {
            pact = pkgs.haskell.lib.addBuildDepend super.pact pkgs.z3;
            aeson = self.callHackage "aeson" "1.1.2.0" {};
            blake2 = guardGhcjs super.blake2;
            haskeline = guardGhcjs super.haskeline;

            # Needed to build with the below version of statistics
            criterion = self.callCabal2nix "criterion" (pkgs.fetchFromGitHub {
              owner = "bos";
              repo = "criterion";
              rev = "5a704392b670c189475649c32d05eeca9370d340";
              sha256 = "1kp0l78l14w0mmva1gs9g30zdfjx4jkl5avl6a3vbww3q50if8pv";
            }) {};

            # Version 1.6.4, needed by weeder, not in callHackage yet
            extra = pkgs.haskell.lib.dontCheck (self.callCabal2nix "extra" (pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "extra";
              rev = "4064bfa7e48a7f1b79f791560d51dbefed879219";
              sha256 = "1p7rc5m70rkm1ma8gnihfwyxysr0n3wxk8ijhp6qjnqp5zwifhhn";
            }) {});

            # 1.20.2 not available to callHackage yet
            haskell-src-exts = self.callCabal2nix "haskell-src-exts" (pkgs.fetchFromGitHub {
              owner = "haskell-suite";
              repo = "haskell-src-exts";
              rev = "4954767e371590ba11ef1edccf904afc9b94f78c";
              sha256 = "1wy7vqk0w4wx12v803w16dbq64gki7i8xvx7d1ibvlwpw4j4pr8f";
            }) {};
            # needed to match haskell-src-exts
            haskell-src-meta = self.callHackage "haskell-src-meta" "0.8.0.2" {};
            # needed to match haskell-src-exts
            haskell-src-exts-util = self.callHackage "haskell-src-exts-util" "0.2.2" {};
            hlint = self.callHackage "hlint" "2.0.14" {};
            hoogle = self.callHackage "hoogle" "5.0.15" {};

            # sbv >= 7.9
            sbv = pkgs.haskell.lib.dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "sbv";
              rev = "3dc60340634c82f39f6c5dca2b3859d10925cfdf";
              sha256 = "18xcxg1h19zx6gdzk3dfs87447k3xjqn40raghjz53bg5k8cdc31";
            }) {});

            # Most recent github commit.  Might also work with 0.14.0.2
            statistics = pkgs.haskell.lib.dontCheck (self.callCabal2nix "statistics" (pkgs.fetchFromGitHub {
              owner = "bos";
              repo = "statistics";
              rev = "1ed1f2844c5a2209f5ea72e60df7d14d3bb7ac1a";
              sha256 = "1jjmdhfn198pfl3k5c4826xddskqkfsxyw6l5nmwrc8ibhhnxl7p";
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
    toolOverrides = ghc: super: {
      z3 = pkgs.z3;
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["pact"];
      # ghcjs = ["pact"];
    };
  
  })
