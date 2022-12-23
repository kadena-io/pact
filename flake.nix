{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=7a94fcdda304d143f9a40006c033d7e190311b54";
    # nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.pact.flake {
        # crossPlatforms = p: [ p.ghcjs ];
      };

      haskellOverlay = (final: prev: {
          pact =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              modules = [{
                # Replace `extra-libraries` dependencies
                packages.mysql.components.library.libs = pkgs.lib.mkForce (with pkgs;
                    [ libmysqlclient ]);
                packages.mysql-simple.components.library.libs = pkgs.lib.mkForce (with pkgs;
                    [ libmysqlclient ]);
              }];
              shell.tools = {
                cabal = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pcre
                libmysqlclient
                pkgconfig
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
          haskell-nix = prev.haskell-nix // {
            extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
            # String pkgconfig-depends names are mapped to lists of Nixpkgs
            # package names
            "mysqlclient" = [ "libmysqlclient" ];
            "mysql" = [ "libmysqlclient" ];
          };
    };

      }
      );

      overlays = [ haskellNix.overlay haskellOverlay];
    in flake // {
      packages.default = builtins.trace (builtins.attrNames flake.packages) flake.packages."pact:exe:pact";
      overlay = haskellOverlay;
    });
}
