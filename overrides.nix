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
  swagger2 = whenGhcjs dontCheck super.swagger2;
  tdigest = whenGhcjs dontCheck super.tdigest;
  temporary = whenGhcjs dontCheck super.temporary;
  trifecta = whenGhcjs dontCheck super.trifecta;
  unix-time = whenGhcjs dontCheck super.unix-time;
  wai-app-static = whenGhcjs dontCheck super.wai-app-static;
  wai-extra = whenGhcjs dontCheck super.wai-extra;

  megaparsec = dontCheck (callHackageDirect {
    pkg = "megaparsec";
    ver = "7.0.5";
    sha256 = "1wizfz8vdplz3sf81vh33sny6p8ynhlpvjxqjpsym7ssb186h0f1";
  });

  neat-interpolation = dontCheck (callHackageDirect {
    pkg = "neat-interpolation";
    ver = "0.3.2.4";
    sha256 = "0gygd2f0wbqa668dz7k8jfryilmbyzravlz7ysp3d13n8h00irba";
  });

  sbv = dontCheck (callHackageDirect {
    pkg = "sbv";
    ver = "8.2";
    sha256 = "1isa8p9dnahkljwj0kz10119dwiycf11jvzdc934lnjv1spxkc9k";
  });

  # Our own custom fork
  thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
    owner = "kadena-io";
    repo = "thyme";
    rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
    sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
  }) {});

  versions = dontCheck (doJailbreak (callHackageDirect {
    pkg = "versions";
    ver = "3.4.0.1";
    sha256 = "1qr6kgkmd065a4wcl653bz65raivkb47i7k6jczc05nciccb7i2l";
  }));

}
