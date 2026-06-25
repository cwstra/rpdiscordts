let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc94.callPackage ./default.nix { }
