let
  pkgs = import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "4df3426f5a5e78cef4835897a43abd9e2a092b74";
    sha256 = "05k5mssiqxffxi45mss9wjns6k76i248rpasa48akdcriry1mp63";
  }) {};
  overrides = self: super: {
        haskell-src-exts  = self.haskell-src-exts_1_20_2;

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

        algebraic-graphs = self.callCabal2nix "algebraic-graphs" algebraic-graphs-src {};
        megaparsec = pkgs.haskell.lib.dontCheck (self.callCabal2nix "megaparsec" megaparsec-src {});
        parser-combinators = self.callCabal2nix "parser-combinators" parser-combinators-src {};
      };
  algebraic-graphs-src = pkgs.fetchFromGitHub {
    owner = "snowleopard";
    repo = "alga";
    rev = "1c04f5664b9476e0b01a573b40462531e52e8756";
    sha256 = "0j121551zqjrp4xy0qcz1pk46znr6w59jkg75v5svdh9ag3vmbsp";
  };

  megaparsec-src = pkgs.fetchFromGitHub {
    owner  = "mrkkrp";
    repo   = "megaparsec";
    rev    = "7b271a5edc1af59fa435a705349310cfdeaaa7e9";
    sha256 = "0415z18gl8dgms57rxzp870dpz7rcqvy008wrw5r22xw8qq0s13c";
  };

  parser-combinators-src = pkgs.fetchFromGitHub {
    owner  = "mrkkrp";
    repo   = "parser-combinators";
    rev    = "dd6599224fe7eb224477ef8e9269602fb6b79fe0";
    sha256 = "11cpfzlb6vl0r5i7vbhp147cfxds248fm5xq8pwxk92d1f5g9pxm";
  };
  pact-src = builtins.filterSource
    (path: type: !(builtins.elem (baseNameOf path)
      ["result" "dist" "dist-ghcjs" ".git" ".stack-work"]))
    ./.;
  pact = pkgs.haskell.lib.addBuildDepend ((pkgs.haskellPackages.extend overrides).callCabal2nix "pact" pact-src {}) pkgs.z3;
in
  if pkgs.lib.inNixShell then pact.env else pact
