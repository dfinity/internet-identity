{ mkDerivation
, base
, bytestring
, criterion
, deepseq
, memory
, random-bytestring
, stdenv
, tasty
, tasty-hunit
, text
}:
mkDerivation {
  pname = "base32";
  version = "0.1.1.2";
  sha256 = "0e6211a58cccbaae9c583c800d99db421cdb259170693a82ad9aa2afd795dfd6";
  libraryHaskellDepends = [ base bytestring text ];
  testHaskellDepends = [
    base
    bytestring
    memory
    random-bytestring
    tasty
    tasty-hunit
    text
  ];
  benchmarkHaskellDepends = [
    base
    bytestring
    criterion
    deepseq
    memory
    random-bytestring
    text
  ];
  homepage = "https://github.com/emilypi/base32";
  description = "RFC 4648-compliant Base32 encodings/decodings";
  license = stdenv.lib.licenses.bsd3;
}
