let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc96.callPackage ./default.nix { }
