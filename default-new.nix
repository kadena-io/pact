{ compiler ? "ghc8107"
, rev      ? "7a94fcdda304d143f9a40006c033d7e190311b54"
, sha256   ? "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik"
, pkgs     ?
    import (builtins.fetchTarball {
      url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      inherit sha256; }) {
      config.allowBroken = false;
      config.allowUnfree = true;
    }
, returnShellEnv ? false
, mkDerivation ? null
}:
let gitignoreSrc = import (pkgs.fetchFromGitHub {
      owner = "hercules-ci";
      repo = "gitignore";
      rev = "9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
      sha256 = "04n9chlpbifgc5pa3zx6ff3rji9am6msrbn1z3x1iinjz2xjfp4p";
    }) {};
    nix-thunk = import ./deps/nix-thunk {};
in
pkgs.haskell.packages.${compiler}.developPackage {
  name = "pact";
  root = gitignoreSrc.gitignoreSource ./.;

  cabal2nixOptions = "--flag=build-tool";

  overrides = self: super: with pkgs.haskell.lib; {
      pretty-simple = dontCheck (self.callHackageDirect {
        pkg = "pretty-simple";
        ver = "3.3.0.0";
        sha256 = "19zwzzvjgadmzp9gw235bsr6wkljr8k0cqy75h5q8n0d5m60ip7m";
      } {});

      direct-sqlite = self.callHackageDirect {
        pkg = "direct-sqlite";
        ver = "2.3.27";
        sha256 = "0w8wj3210h08qlws40qhidkscgsil3635zk83kdlj929rbd8khip";
       } {};

      sbv = dontCheck (self.callHackageDirect {
        pkg = "sbv";
        ver = "9.0";
        sha256 = "14g2qax1vc7q4g78fa562dviqvcd0l52kd5jmgv90g3g3ci15bnl";
      } {});

      statistics = dontCheck (self.callHackageDirect {
        pkg = "statistics";
        ver = "0.15.2.0";
        sha256 = "1sg1gv2sc8rdsl6qby6p80xv3iasy6w2khbkc6cx7j2iva67v33r";
      } {});

      hashable = self.callHackageDirect {
        pkg = "hashable";
        ver = "1.4.1.0";
        sha256 = "0ms8df9v4rcy424ggsjaz9ik4fnggs6698zlfq099vqmsp2x93nn";
      } {};

      pact-time = self.callHackageDirect {
        pkg = "pact-time";
        ver = "0.2.0.1";
        sha256 = "0lp0nypq675f652k2pl4kf92agy9jhzvf03zx61gvj4djgjw1rxy";
      } {};

      lens-aeson = self.callHackageDirect {
        pkg = "lens-aeson";
        ver = "1.2.2";
        sha256 = "154lx61zbdgnmd9csnbvfqzpv4y9lqm7k7ssal2d4mxlwwlcx6jf";
      } {};

      rebase       = doJailbreak super.rebase;
      ralist       = markUnbroken (doJailbreak super.ralist);

      # Cuckoo tests fail due to a missing symbol
      cuckoo        = dontCheck super.cuckoo;

      # These tests pull in unnecessary dependencies
      http2         = dontCheck super.http2;
      prettyprinter = dontCheck super.prettyprinter;
      aeson         = dontCheck super.aeson;
      generic-data  = dontCheck super.generic-data;
  };

  source-overrides = {
    # OneTuple                    = "0.3";
    ansi-terminal               = "0.11.3";
    prettyprinter-ansi-terminal = "1.1.2";
    # time-compat                 = "1.9.5";
    trifecta                    = "2.1.2";
    hspec-golden                = "0.1.0.3";
    pact-time                   = "0.2.0.1";
    unordered-containers        = "0.2.19.1";

    # These are required in order to not break payload validation
    base16-bytestring = "0.1.1.7";
    prettyprinter     = "1.7.1";
    base64-bytestring = "1.0.0.3";
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (
    with pkgs.haskell.packages.${compiler};
    attrs: {
      libraryToolDepends = (attrs.libraryToolDepends or []) ++ [
        happy
        alex
      ];
      buildTools = (attrs.buildTools or []) ++ [
        pkgs.zlib
        pkgs.z3
        pkgs.pkgconfig
        cabal-install
      ];
    });

  inherit returnShellEnv;
}
