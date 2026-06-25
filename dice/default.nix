{ mkDerivation, aeson, attoparsec, base, ghc, hashable, hspec
, hspec-contrib, hspec-discover, integer-roots, lib, mtl
, parser-combinators, QuickCheck, random, relude, scotty, text
, unordered-containers
}:
mkDerivation {
  pname = "rpdiscorddice";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base ghc hashable integer-roots mtl parser-combinators
    random relude text unordered-containers
  ];
  executableHaskellDepends = [ aeson base random relude scotty ];
  testHaskellDepends = [
    attoparsec base hspec hspec-contrib integer-roots
    parser-combinators QuickCheck random relude text
  ];
  testToolDepends = [ hspec-discover ];
  license = "unknown";
  mainProgram = "rpdiscorddice";
}
