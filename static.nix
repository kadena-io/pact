{ system ? builtins.currentSystem }:

let
  rp = import (builtins.fetchTarball {
    url = "https://github.com/vaibhavsagar/reflex-platform/archive/ae542c3e7ed4fb1b4552f447b1205982e261cd68.tar.gz";
    sha256 = "0p14b4kdjkykkcql8xdp2x8qvw7cla8imikl940a8qcsc49vkwpf";
  }) { inherit system; };
in (rp.ghcMusl64.override {
  overrides = import ./staticOverrides.nix rp.nixpkgs;
}).pact
