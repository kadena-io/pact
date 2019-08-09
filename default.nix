{ rpRef ? "cfbbd5205019febade3e52ed479490d66e621995"
, rpSha ?  "1r3kcvy0zf6qdvnasins43hb77lpn0r26vvmnh3cf3yrvpy86sam"
, system ? builtins.currentSystem
}:

let rp = builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
  sha256 = rpSha;
};
overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "e79542cc689d52ec4cb34ce4ae3fbe56e7a0bf70";
      sha256 = "11sy98clv7ln0a5vqxzvh6wwqbswsjbik2084hav5kfws4xvklfa";
    };
  });
};

in
  (import rp { inherit system; nixpkgsOverlays = [ overlay ]; }).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
        sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
      }) {};
  in
  rec {

    name = "pact";
    overrides = self: super: with pkgs.haskell.lib;
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
          whenGhcjs = f: p: if self.ghc.isGhcjs or false then (f p) else p;
          callHackageDirect = {pkg, ver, sha256}@args:
            let pkgver = "${pkg}-${ver}";
            in self.callCabal2nix pkg (pkgs.fetchzip {
                 url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
                 inherit sha256;
               }) {};
       in {
            pact = doCoverage (addBuildDepend super.pact pkgs.z3);
            haskeline = guardGhcjs super.haskeline;

            # tests for extra were failing due to an import clash (`isWindows`)
            extra = dontCheck super.extra;

            base-compat-batteries = dontCheck super.base-compat-batteries;

            # tests try to use ghc-pkg and cabal (https://github.com/sol/doctest/issues/213)
            doctest = guardGhcjs (dontCheck (self.callHackage "doctest" "0.16.0" {}));
            # these want to use doctest, which doesn't work on ghcjs
            bytes = dontCheck super.bytes;
            intervals = dontCheck super.intervals;
            bound = dontCheck super.bound;
            trifecta = dontCheck super.trifecta;
            lens-aeson = dontCheck super.lens-aeson;
            # test suite for this is failing on ghcjs:
            hw-hspec-hedgehog = dontCheck super.hw-hspec-hedgehog;

            bsb-http-chunked = whenGhcjs dontCheck super.bsb-http-chunked;
            Glob = whenGhcjs dontCheck super.Glob;
            http2 = whenGhcjs dontCheck super.http2;
            http-date = whenGhcjs dontCheck super.http-date;
            http-media = whenGhcjs dontCheck super.http-media;
            iproute = whenGhcjs dontCheck super.iproute;
            markdown-unlit = whenGhcjs dontCheck super.markdown-unlit;
            mockery = whenGhcjs dontCheck super.mockery;
            silently = whenGhcjs dontCheck super.silently;
            servant = whenGhcjs dontCheck super.servant;
            servant-client = whenGhcjs dontCheck super.servant-client;
            servant-server = whenGhcjs dontCheck super.servant-server;
            servant-swagger = whenGhcjs dontCheck super.servant-swagger;
            unix-time = whenGhcjs dontCheck super.unix-time;
            wai-app-static = whenGhcjs dontCheck super.wai-app-static;
            wai-extra = whenGhcjs dontCheck super.wai-extra;

            foundation = pkgs.lib.flip whenGhcjs super.foundation (package: overrideCabal package (drv: {
              postPatch = (drv.postPatch or "") + pkgs.lib.optionalString (system == "x86_64-darwin") ''
                substituteInPlace foundation.cabal --replace 'if os(linux)' 'if os(linux) && !impl(ghcjs)'
                substituteInPlace foundation.cabal --replace 'if os(osx)' 'if os(linux) && impl(ghcjs)'
              '';
            }));

            algebraic-graphs = dontCheck super.algebraic-graphs;

            swagger2 = whenGhcjs dontCheck (doJailbreak (callHackageDirect {
              pkg = "swagger2";
              ver = "2.3.1.1";
              sha256 = "0rhxqdiymh462ya9h76qnv73v8hparwz8ibqqr1d06vg4zm7s86p";
            }));

            # Prevent: "Setup: Encountered missing dependencies: doctest >=0.9"
            prettyprinter = dontCheck super.prettyprinter;
            prettyprinter-ansi-terminal = dontCheck super.prettyprinter-ansi-terminal;
            prettyprinter-convert-ansi-wl-pprint = dontCheck super.prettyprinter-convert-ansi-wl-pprint;

            # Needed to get around a requirement on `hspec-discover`.
            megaparsec = dontCheck super.megaparsec;

            hedgehog = self.callCabal2nix "hedgehog" (pkgs.fetchFromGitHub {
              owner = "hedgehogqa";
              repo = "haskell-hedgehog";
              rev = "38146de29c97c867cff52fb36367ff9a65306d76";
              sha256 = "1z8d3hqrgld1z1fvjd0czksga9lma83caa2fycw0a5gfbs8sh4zh";
            } + "/hedgehog") {};
            hlint = self.callHackage "hlint" "2.0.14" {};
            # hoogle = self.callHackage "hoogle" "5.0.15" {};

            sbv = dontCheck (callHackageDirect {
              pkg = "sbv";
              ver = "8.2";
              sha256 = "1isa8p9dnahkljwj0kz10119dwiycf11jvzdc934lnjv1spxkc9k";
            });

            # need crackNum 2.3
            crackNum = pkgs.haskell.lib.dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "crackNum";
              rev = "54cf70861a921062db762b3c50e933e73446c3b2";
              sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
            }) {});

            # Our own custom fork
            thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {});

            # aeson 1.4.2
            aeson = (if self.ghc.isGhcjs or false
              then (pkgs.lib.flip addBuildDepend self.hashable-time)
              else pkgs.lib.id) (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
              owner = "bos";
              repo = "aeson";
              rev = "c3d04181eb64393d449a68084ffea3a94c3d8064";
              sha256 = "1l8lks6plj0naj9ghasmkqglshxym3f29gyybvjvkrkm770p2gl4";
            }) {});
          };
    packages = {
      pact = gitignore.gitignoreSource [".git" ".gitlab-ci.yml" "CHANGELOG.md" "README.md"] ./.;
    };
    shellToolOverrides = ghc: super: {
      z3 = pkgs.z3;
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["pact"];
      ghcjs = ["pact"];
    };
    passthru = { inherit overrides; };

  })
