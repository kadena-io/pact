pkgs: hackGet: self: super: with pkgs.haskell.lib;
let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
    whenGhcjs = f: p: if self.ghc.isGhcjs or false then (f p) else p;
    callHackageDirect = {pkg, ver, sha256}@args:
      let pkgver = "${pkg}-${ver}";
      in self.callCabal2nix pkg (pkgs.fetchzip {
           url = "http://hackage.haskell.org/package/${pkgver}/${pkgver}.tar.gz";
           inherit sha256;
         }) {};

    # includes servant-jsaddle (needed for chainweaver)
    servantSrc = pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant";
      rev = "925d50d7526a9b95918b7a2d49e57afa10985302";
      sha256 = "0zzchj9pc9y50acvj8zbm94bgbvbxzxz2b0xd2zbck90bribwm5b";
    };
in {
  pact = doCoverage (addBuildDepend super.pact pkgs.z3);

  Glob = whenGhcjs dontCheck super.Glob;

  aeson = if self.ghc.isGhcjs or false
    then dontCheck (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "aeson";
        rev = "d6288c431a477f9a6e93aa80454a9e1712127548"; # branch v1450-text-jsstring containing (ToJSVal Value) instance and other fixes
        sha256 = "102hj9b42z1h9p634g9226nvs756djwadrkz9yrb15na671f2xf4";
      }) {})
    else enableCabalFlag (dontCheck (callHackageDirect {
        pkg = "aeson";
        ver = "1.4.5.0";
        sha256 = "0imcy5kkgrdrdv7zkhkjvwpdp4sms5jba708xsap1vl9c2s63n5a";
      })) "cffi";

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
  memory = whenGhcjs dontCheck super.memory;
  network-byte-order = whenGhcjs dontCheck super.network-byte-order;
  prettyprinter-ansi-terminal = whenGhcjs dontCheck super.prettyprinter-ansi-terminal;
  prettyprinter-convert-ansi-wl-pprint = whenGhcjs dontCheck super.prettyprinter-convert-ansi-wl-pprint;
  tdigest = whenGhcjs dontCheck super.tdigest;
  temporary = whenGhcjs dontCheck super.temporary;
  text-short = whenGhcjs dontCheck super.text-short; # either hang or take a long time
  unix-time = whenGhcjs dontCheck super.unix-time;
  wai-app-static = whenGhcjs dontCheck super.wai-app-static;
  wai-extra = whenGhcjs dontCheck super.wai-extra;

  base-orphans = dontCheck (callHackageDirect {
    pkg = "base-orphans";
    ver = "0.8.1";
    sha256 = "1jg06ykz8fsk1vlwih4vjw3kpcysp8nfsv7qjm42y2gfyzn6jvsk";
  });

  dec = dontCheck (callHackageDirect {
    pkg = "dec";
    ver = "0.0.3";
    sha256 = "11b8g4nm421pr09yfb4zp18yb7sq4wah598fi3p5fb64yy4c2n4s";
  });

  hedgehog = dontCheck (callHackageDirect {
    pkg = "hedgehog";
    ver = "1.0.1";
    sha256 = "0h9qwd4gw5n8j8is9kn9mll32c8v6z1dv9mp4fmkmz7k5zi4asjq";
  });

  http-api-data = dontCheck (callHackageDirect {
    pkg = "http-api-data";
    ver = "0.4.1";
    sha256 = "0wqji0raiq3snh7yifmv754sg5zjvw2gisgz1d3d0ljib2sw4jiq";
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

  sbv = dontCheck (callHackageDirect {
    pkg = "sbv";
    ver = "8.2";
    sha256 = "1isa8p9dnahkljwj0kz10119dwiycf11jvzdc934lnjv1spxkc9k";
  });

  # https://github.com/reflex-frp/reflex-platform/issues/549
  singleton-bool =
    if self.ghc.isGhcjs or false
    then overrideCabal (self.callCabal2nix "singleton-bool" (pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "singleton-bool";
        rev = "bf5c81fff6eaa9ed1286de9d0ecfffa7e0aa85d2";
        sha256 = "0fzi6f5pl2gg9k8f7k88qyyvjflpcw08905y0vjmbylzc70wsykw";
      }) {})
      (drv: {
        editedCabalFile = null;
        revision = null;
      })
    else dontCheck (callHackageDirect {
        pkg = "singleton-bool";
        ver = "0.1.5";
        sha256 = "1kjn5wgwgxdw2xk32d645v3ss2a70v3bzrihjdr2wbj2l4ydcah1";
      });

  servant = dontCheck (self.callCabal2nix "servant" "${servantSrc}/servant" {});
  servant-client = dontCheck (self.callCabal2nix "servant-client" "${servantSrc}/servant-client" {});
  servant-client-core = dontCheck (self.callCabal2nix "servant-client-core" "${servantSrc}/servant-client-core" {});
  servant-server = dontCheck (self.callCabal2nix "servant-server" "${servantSrc}/servant-server" {});

  servant-swagger = dontCheck (callHackageDirect {
    pkg = "servant-swagger";
    ver = "1.1.7.1";
    sha256 = "1ymdcmdi234p9jbwa7rgj1j35n9xnx4kgfjba4gs2r8cnhqwak28";
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

  time-compat = dontCheck (callHackageDirect {
    pkg = "time-compat";
    ver = "1.9.2.2";
    sha256 = "11kdcw1g8m9hl6ps9i8hqrcpgidmv0r19sbxcwm1qrp9wf0bfq1y";
  });

  trifecta = dontCheck (callHackageDirect {
    pkg = "trifecta";
    ver = "2.1";
    sha256 = "0hbv8q12rgg4ni679fbx7ac3blzqxj06dw1fyr6ipc8kjpypb049";
  });

  unordered-containers = dontCheck (callHackageDirect {
    pkg = "unordered-containers";
    ver = "0.2.10.0";
    sha256 = "16xpq9qb1ipl0mb86rlb3bx29xvgcwirpm2ds0ynxjh0ylwzavkk";
  });

  hspec-golden = dontCheck (callHackageDirect {
    pkg = "hspec-golden";
    ver = "0.1.0.1";
    sha256 = "1fplsb3rb6f3w20cncr0zrjpf7x4kc3njy8l016p5wxxh3hkgdrs";
  });

  neat-interpolation = dontCheck (callHackageDirect {
    pkg = "neat-interpolation";
    ver = "0.5.0.1";
    sha256 = "10ss12dvgrcbgp4kh1g0i91h30m79kl4v2k37k18lv9q06kjhx5j";
  });
}
