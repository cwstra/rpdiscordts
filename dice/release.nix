let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc98.callPackage ./default.nix { }
