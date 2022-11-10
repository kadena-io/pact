{
  description = "Kadena's Pact smart contract language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?rev=7a94fcdda304d143f9a40006c033d7e190311b54";
    hercules-ci.url = "github:hercules-ci/gitignore?rev=9e80c4d83026fa6548bc53b1a6fab8549a6991f6";
  };

  outputs = { self, nixpkgs, hercules-ci, returnShellEnv ? false }: let
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
  in {
    packages = forAllSystems (system:
      let pkgs = nixpkgsFor.${system};
          gitignoreSrc = import hercules-ci {};
      in {

      pact = pkgs.haskell.packages.${compiler}.developPackage {
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

            hashable     = doJailbreak super.hashable;
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
          OneTuple                    = "0.3";
          aeson                       = "1.5.6.0";
          ansi-terminal               = "0.11.3";
          prettyprinter-ansi-terminal = "1.1.2";
          time-compat                 = "1.9.5";
          trifecta                    = "2.1.1";
          unordered-containers        = "0.2.15.0";
          hspec-golden                = "0.1.0.3";

          # These are required in order to not break payload validation
          base16-bytestring = "0.1.1.7";
          prettyprinter     = "1.6.0";
          hashable          = "1.3.0.0";
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

        meta = {
          homepage = "http://github.com/kadena-io/pact";
          description = "Kadena's Pact smart contract language";
          longDescription = ''
            Kadena's Pact smart contract language
          '';
          platforms = pkgs.lib.platforms.all;
          maintainers = with pkgs.lib.maintainers; [ jwiegley ];
          license = pkgs.lib.licenses.bsd3;
        };
      };
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.pact);
  };
}
