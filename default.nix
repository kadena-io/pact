{ rpRef ? "ea3c9a1536a987916502701fb6d319a880fdec96", rpSha ?  "0339ds5xa4ymc7xs8nzpa4mvm09lzscisdgpdfc6rykwhbgw9w2a" }:

let rp = (import <nixpkgs> {}).fetchFromGitHub {
           owner = "mightybyte";
           repo = "reflex-platform";
           rev = rpRef;
           sha256 = rpSha;
         };

in
  (import rp {}).project ({ pkgs, ... }: {
    name = "pact-umbrella";
    overrides = self: super: {
      hastache = pkgs.haskell.lib.dontCheck super.hastache;
      statistics = pkgs.haskell.lib.dontCheck super.statistics;
      thyme = pkgs.haskell.lib.enableCabalFlag (pkgs.haskell.lib.dontCheck super.thyme) "ghcjs";
    };
    packages = {
      pact = ./pact;
      pact-server = ./pact-server;
      pact-ghcjs = ./pact-ghcjs;

      cabal-doctest = pkgs.fetchFromGitHub {
        owner = "phadej";
        repo = "cabal-doctest";
        rev = "2892c83df9f83c19481908568ad8508ab0b3c8a5";
        sha256 = "03xm1g7s1ld27bxcy4l8hcy44565bszz5sgppinaqfzk928q5hmj";
      };
      criterion = pkgs.fetchFromGitHub {
        owner = "bos";
        repo = "criterion";
        rev = "5a704392b670c189475649c32d05eeca9370d340";
        sha256 = "1kp0l78l14w0mmva1gs9g30zdfjx4jkl5avl6a3vbww3q50if8pv";
      };
      haskeline = pkgs.fetchFromGitHub {
        owner = "judah";
        repo = "haskeline";
        rev = "481b890e8616d9e85da69173d5079ad49f9fb83d";
        sha256 = "1xpnxbr86789zf5adsapzsra9bq9mfhcgzr8xzqm3wv5v8nbvd5n";
      };
      hastache = pkgs.fetchFromGitHub {
        owner = "lymar";
        repo = "hastache";
        rev = "8201405035de8d32247c4e32185e7767dcb4075b";
        sha256 = "1h8n8df4kyh4arfkfl8yn1acjj3vvyhyfi17dp31v3pbzasph3z7";
      };
      heist = pkgs.fetchFromGitHub {
        owner = "snapframework";
        repo = "heist";
        rev = "3ccbec548830abce7ed7eba42c1c294b02b6cd52";
        sha256 = "14sd4d4an7fj8yb4mr8cdallsv69x5jb1hd330sg10ahi1ryzspr";
      };
      map-syntax = pkgs.fetchFromGitHub {
        owner = "mightybyte";
        repo = "map-syntax";
        rev = "acebcf0a83ee639e1a0c49850b9c85821d53f621";
        sha256 = "076knpvls1489gish9z30lhb21vqx44k366vc2i3kdql815v1vqv";
      };
      statistics = pkgs.fetchFromGitHub {
        owner = "bos";
        repo = "statistics";
        rev = "1ed1f2844c5a2209f5ea72e60df7d14d3bb7ac1a";
        sha256 = "1jjmdhfn198pfl3k5c4826xddskqkfsxyw6l5nmwrc8ibhhnxl7p";
      };
      thyme = pkgs.fetchFromGitHub {
        owner = "kadena-io";
        repo = "thyme";
        rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
        sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
      };
    };
    
    shells = {
      ghc = ["pact" "pact-server" "pact-ghcjs"];
      ghcjs = ["pact" "pact-ghcjs"];
    };
  
  })
