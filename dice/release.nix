let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc92.callPackage ./default.nix { }
