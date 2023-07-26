{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
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
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc962";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pkgconfig
                python3 python3Packages.sphinx python3Packages.sphinx_rtd_theme
                pandoc perl
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
        })
      ];
      # This package depends on other packages at buildtime, but its output does not
      # depend on them. This way, we don't have to download the entire closure to verify
      # that those packages build.
      mkCheck = name: package: pkgs.runCommand ("check-"+name) {} ''
        echo ${name}: ${package}
        echo works > $out
      '';
    in flake // rec {
      packages.default = flake.packages."pact:exe:pact";
      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact" packages.default}
        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';
    });
}
