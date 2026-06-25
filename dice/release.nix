let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskell.packages.ghc967.callPackage ./default.nix { }
