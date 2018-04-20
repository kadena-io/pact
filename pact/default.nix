# To pin to a specific version of nixpkgs, you can substitute <nixpkgs> with:
# `(builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz")`
{ compiler ? "ghc802"
, nixpkgs     ? import <nixpkgs> {} }:
  nixpkgs.haskell.packages.${compiler}.developPackage {
    root = ./.;
    overrides = self: super: {
      # Don't run a package's test suite
      # thyme = nixpkgs.haskell.lib.dontCheck super.thyme;
      #
      # Don't enforce package's version constraints
      # bar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.bar;
      #
      # To discover more functions that can be used to modify haskell
      # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
      # <TAB> to get a tab-completed list of functions.
    };
    source-overrides = {
      # Use a specific hackage version
      # bytestring = "0.10.8.1";
      #
      # Use a particular commit from github
      # thyme = nixpkgs.fetchFromGitHub {
      #   owner = "kadena-io";
      #   repo = "thyme";
      #   rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
      #   sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
      # };
    };
  }