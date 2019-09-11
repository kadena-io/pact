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

  haskeline = guardGhcjs super.haskeline;

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
