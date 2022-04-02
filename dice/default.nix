{ mkDerivation, base, ghc, hashable, HUnit, integer-roots, lib
, megaparsec, mtl, parser-combinators, QuickCheck, random, relude
, text, unordered-containers
}:
mkDerivation {
  pname = "rpdiscorddice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base ghc hashable HUnit integer-roots megaparsec mtl
    parser-combinators QuickCheck random relude text
    unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
