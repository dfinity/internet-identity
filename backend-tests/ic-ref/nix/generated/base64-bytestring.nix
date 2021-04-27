{ mkDerivation
, base
, bytestring
, containers
, criterion
, deepseq
, HUnit
, QuickCheck
, split
, stdenv
, test-framework
, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "base64-bytestring";
  version = "1.1.0.0";
  sha256 = "210d6c9042241ca52ee5d89cf221dbeb4d0e64b37391345369035ad2d9b4aca9";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base
    bytestring
    containers
    HUnit
    QuickCheck
    split
    test-framework
    test-framework-hunit
    test-framework-quickcheck2
  ];
  benchmarkHaskellDepends = [
    base
    bytestring
    containers
    criterion
    deepseq
  ];
  homepage = "https://github.com/haskell/base64-bytestring";
  description = "Fast base64 encoding and decoding for ByteStrings";
  license = stdenv.lib.licenses.bsd3;
}
