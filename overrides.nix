pkgs: self: super: with pkgs.haskell.lib;
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

  Glob = whenGhcjs dontCheck super.Glob;
  algebraic-graphs = whenGhcjs dontCheck super.algebraic-graphs;
  base-compat-batteries = whenGhcjs dontCheck super.base-compat-batteries;
  bound = whenGhcjs dontCheck super.bound;
  bsb-http-chunked = whenGhcjs dontCheck super.bsb-http-chunked;
  bytes = whenGhcjs dontCheck super.bytes;
  extra = whenGhcjs dontCheck super.extra;
  haskeline = guardGhcjs super.haskeline;
  http-date = whenGhcjs dontCheck super.http-date;
  http-media = whenGhcjs dontCheck super.http-media;
  http2 = whenGhcjs dontCheck super.http2;
  inspection-testing = guardGhcjs super.inspection-testing;
  intervals = whenGhcjs dontCheck super.intervals;
  iproute = whenGhcjs dontCheck super.iproute;
  lens-aeson = whenGhcjs dontCheck super.lens-aeson;
  network-byte-order = whenGhcjs dontCheck super.network-byte-order;
  prettyprinter-ansi-terminal = whenGhcjs dontCheck super.prettyprinter-ansi-terminal;
  prettyprinter-convert-ansi-wl-pprint = whenGhcjs dontCheck super.prettyprinter-convert-ansi-wl-pprint;
  servant = whenGhcjs dontCheck super.servant;
  servant-client = whenGhcjs dontCheck super.servant-client;
  servant-server = whenGhcjs dontCheck super.servant-server;
  tdigest = whenGhcjs dontCheck super.tdigest;
  temporary = whenGhcjs dontCheck super.temporary;
  unix-time = whenGhcjs dontCheck super.unix-time;
  wai-app-static = whenGhcjs dontCheck super.wai-app-static;
  wai-extra = whenGhcjs dontCheck super.wai-extra;

  hedgehog = dontCheck (callHackageDirect {
    pkg = "hedgehog";
    ver = "1.0.1";
    sha256 = "0h9qwd4gw5n8j8is9kn9mll32c8v6z1dv9mp4fmkmz7k5zi4asjq";
  });

  insert-ordered-containers = dontCheck (callHackageDirect {
    pkg = "insert-ordered-containers";
    ver = "0.2.2";
    sha256 = "1md93iaxsr4djx1i47zjwddd7pd4j3hzphj7495q7lz7mn8ifz4w";
  });

  megaparsec = dontCheck (callHackageDirect {
    pkg = "megaparsec";
    ver = "7.0.5";
    sha256 = "1wizfz8vdplz3sf81vh33sny6p8ynhlpvjxqjpsym7ssb186h0f1";
  });

  servant-swagger = dontCheck (callHackageDirect {
    pkg = "servant-swagger";
    ver = "1.1.7.1";
    sha256 = "1ymdcmdi234p9jbwa7rgj1j35n9xnx4kgfjba4gs2r8cnhqwak28";
  });

  sbv = dontCheck (callHackageDirect {
    pkg = "sbv";
    ver = "8.2";
    sha256 = "1isa8p9dnahkljwj0kz10119dwiycf11jvzdc934lnjv1spxkc9k";
  });

  swagger2 = dontCheck (callHackageDirect {
    pkg = "swagger2";
    ver = "2.4";
    sha256 = "1kgajvqbx8627191akn6pz4kiyi24gawvnvkyb7955dy7bnpd9pn";
  });

  tasty-hedgehog = dontCheck (callHackageDirect {
    pkg = "tasty-hedgehog";
    ver = "1.0.0.1";
    sha256 = "06mffkvscl8r81hjhsvjlyqa843szgv8fays1l9z4jaw2759glsr";
  });

  # Our own custom fork
  thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "thyme";
    rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
    sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
  }) {});

  trifecta = dontCheck (callHackageDirect {
    pkg = "trifecta";
    ver = "2.1";
    sha256 = "0hbv8q12rgg4ni679fbx7ac3blzqxj06dw1fyr6ipc8kjpypb049";
  });
}
