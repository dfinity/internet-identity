{ mkDerivation
, base
, containers
, deepseq
, process
, random
, splitmix
, stdenv
, template-haskell
, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.14.2";
  sha256 = "d87b6c85696b601175274361fa62217894401e401e150c3c5d4013ac53cd36f3";
  libraryHaskellDepends = [
    base
    containers
    deepseq
    random
    splitmix
    template-haskell
    transformers
  ];
  testHaskellDepends = [ base deepseq process ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
