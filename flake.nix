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
                (python3.withPackages (ps: [ps.sphinx ps.sphinx_rtd_theme]))
                pandoc perl
              ];
              # shell.crossPlatforms = p: [ p.ghcjs ];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."pact:exe:pact";

      packages.docs = pkgs.stdenv.mkDerivation {
        name = "pact-docs";
        src = ./docs;
        buildInputs = [
          (pkgs.python3.withPackages (ps: [ps.sphinx ps.sphinx_rtd_theme]))
          pkgs.pandoc
          pkgs.perl
        ];
        buildPhase = ''
          source ./work.sh
          mkdir $out
          cp -r _build/* $out
        '';
      };

      devShell = pkgs.haskellPackages.shellFor {
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          # hlint
        ];

        withHoogle = true;
      };
    });
}
