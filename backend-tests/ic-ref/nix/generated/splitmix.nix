{ mkDerivation
, async
, base
, base-compat
, base-compat-batteries
, bytestring
, clock
, containers
, criterion
, deepseq
, HUnit
, math-functions
, process
, random
, stdenv
, test-framework
, test-framework-hunit
, tf-random
, vector
}:
mkDerivation {
  pname = "splitmix";
  version = "0.1.0.3";
  sha256 = "46009f4b000c9e6613377767b8718bf38476469f2a8e2162d98cc246882d5a35";
  libraryHaskellDepends = [ base deepseq ];
  testHaskellDepends = [
    async
    base
    base-compat
    base-compat-batteries
    bytestring
    containers
    deepseq
    HUnit
    math-functions
    process
    random
    test-framework
    test-framework-hunit
    tf-random
    vector
  ];
  benchmarkHaskellDepends = [
    base
    clock
    containers
    criterion
    random
    tf-random
  ];
  description = "Fast Splittable PRNG";
  license = stdenv.lib.licenses.bsd3;
}
