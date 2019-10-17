{ rpRef ? "9afc9a2a1864e5a89981a4959a88c646e7441549"
, rpSha ? "0rkc6h4mfi5j4d4735b0l7ic6dvww4vdxjwd3kninj5rjlkddhfd"
, system ? builtins.currentSystem
}:

let rpSrc = builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
  sha256 = rpSha;
};
overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    name = "z3-4.8.7-head";
    version = "4.8.7-head";
    patches = [];
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "4ce6b53d95821a2cfcb46a579bc2561b2101689f";
      sha256 = "1dnkfdyvwznbgwf2b637svg0fxg66dpn90bqvfypnys5y46cqq84";
    };
  });
};
rp = import rpSrc { inherit system; nixpkgsOverlays = [ overlay ]; };
in {
  inherit rpRef rpSha rpSrc overlay rp;
  proj =
    rp.project ({ pkgs, ... }:
    let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
          owner = "siers";
          repo = "nix-gitignore";
          rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
          sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
        }) {};
    in
    {
      name = "pact";
      overrides = self: super: (import ./overrides.nix pkgs self super) // {
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

    });
}
