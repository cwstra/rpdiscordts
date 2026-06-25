let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc910.callPackage ./default.nix { }
