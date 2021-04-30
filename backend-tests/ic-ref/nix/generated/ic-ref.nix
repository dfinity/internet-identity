# THIS IS AN AUTOMATICALLY GENERATED FILE. DO NOT EDIT MANUALLY!
# See ./nix/generate.nix for instructions.

{ mkDerivation
, pkgs
, aeson
, asn1-encoding
, asn1-types
, atomic-write
, base
, base32
, base64-bytestring
, binary
, bindings-DSL
, bytestring
, candid
, cborg
, cereal
, containers
, crc
, cryptonite
, data-default-class
, directory
, ed25519
, filepath
, hashable
, hex-text
, http-client
, http-client-tls
, http-types
, leb128-cereal
, memory
, MonadRandom
, mtl
, optparse-applicative
, parallel
, prettyprinter
, primitive
, process
, QuickCheck
, quickcheck-io
, random
, row-types
, serialise
, split
, splitmix
, stdenv
, tasty
, tasty-ant-xml
, tasty-html
, tasty-hunit
, tasty-quickcheck
, tasty-rerun
, template-haskell
, temporary
, text
, time
, transformers
, unordered-containers
, utf8-string
, vector
, wai
, wai-extra
, warp
, winter
, zlib
}:
mkDerivation {
  pname = "ic-ref";
  version = "0.0.1";
  src = import ../gitSource.nix "impl";
  configureFlags = [ "-frelease" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
    atomic-write
    base
    base32
    base64-bytestring
    binary
    bindings-DSL
    bytestring
    candid
    cborg
    cereal
    containers
    crc
    cryptonite
    data-default-class
    directory
    ed25519
    filepath
    hashable
    hex-text
    http-client
    http-client-tls
    http-types
    leb128-cereal
    memory
    MonadRandom
    mtl
    optparse-applicative
    parallel
    prettyprinter
    primitive
    process
    random
    row-types
    serialise
    split
    splitmix
    tasty
    tasty-ant-xml
    tasty-html
    tasty-hunit
    tasty-rerun
    template-haskell
    text
    time
    transformers
    unordered-containers
    utf8-string
    vector
    wai
    wai-extra
    warp
    winter
    zlib
  ];
  testHaskellDepends = [
    aeson
    asn1-encoding
    asn1-types
    atomic-write
    base
    base32
    base64-bytestring
    binary
    bindings-DSL
    bytestring
    candid
    cborg
    cereal
    containers
    crc
    cryptonite
    data-default-class
    directory
    ed25519
    filepath
    hashable
    hex-text
    leb128-cereal
    memory
    MonadRandom
    mtl
    parallel
    primitive
    QuickCheck
    quickcheck-io
    random
    row-types
    serialise
    split
    splitmix
    tasty
    tasty-hunit
    tasty-quickcheck
    temporary
    text
    time
    transformers
    unordered-containers
    utf8-string
    vector
    winter
    zlib
  ];
  doCheck = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
