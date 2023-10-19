{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    # nixpkgs.url = "github:NixOS/nixpkgs?rev=4d2b37a84fad1091b9de401eb450aae66f1a741e";
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
              compiler-nix-name = "ghc981";
              shell.tools = {
                # cabal = {};
                # haskell-language-server = {};
                # hlint = {};
              };
              modules = [{ghcOptions = ["-Wno-error=x-partial"];}];
              shell.buildInputs = with pkgs; [
                zlib
                z3
                pkg-config
                (python3.withPackages (ps: [ps.sphinx ps.sphinx_rtd_theme]))
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
        '';
        installPhase = ''
          mkdir $out
          cp -r _build/* $out
        '';
      };

      devShell = pkgs.haskellPackages.shellFor {
        buildInputs = with pkgs.haskellPackages; [
          # haskell-language-server
          # hlint
        ];

        withHoogle = true;
      };
      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact" packages.default}
        echo ${mkCheck "devShell" flake.devShell}
        echo works > $out
      '';

    });
}
