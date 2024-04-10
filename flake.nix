{
  description = "Kadena's Pact smart contract language";

  inputs = {
    hs-nix-infra.url = "github:kadena-io/hs-nix-infra";
    flake-utils.url = "github:numtide/flake-utils";
    nix-bundle-exe = { url = "github:3noch/nix-bundle-exe"; flake = false; };
  };

  outputs = inputs@{ self, hs-nix-infra, flake-utils, ...}:
    flake-utils.lib.eachSystem
      [ "x86_64-linux" "x86_64-darwin"
        "aarch64-linux" "aarch64-darwin" ] (system:
    let
      inherit (hs-nix-infra) nixpkgs haskellNix;
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      project = pkgs.pact;
      flake = project.flake {
        # crossPlatforms = p: [ p.ghcjs ];
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pact =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc964";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
                z3_4_11
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
    in rec {
      packages.default = flake.packages."pact:exe:pact";
      packages.recursive = with hs-nix-infra.lib.recursive system;
        wrapRecursiveWithMeta "pact" "${wrapFlake self}.default";

      packages.pact-binary-bundle = pkgs.callPackage inputs.nix-bundle-exe {}
        packages.default;

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

      inherit (flake) devShell;

      packages.check = pkgs.runCommand "check" {} ''
        echo ${mkCheck "pact" packages.default}
        echo ${mkCheck "devShell" devShell}
        echo works > $out
      '';

      # Other flake outputs provided by haskellNix can be accessed through
      # this project output
      inherit project;
    });
}
