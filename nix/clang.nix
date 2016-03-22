   let
     pkgs = import <nixpkgs> {};
     stdenv = pkgs.stdenv;
   in rec {
     clangEnv = stdenv.mkDerivation rec {
       name = "clang-env";
       version = "1.1.1.1";
       src = ./.;
       buildInputs = [ pkgs.clang ];
     };
   }

