nix: subpath:
  self: super: {
  winter = super.callPackage generated/winter.nix {};
  ic-ref = super.callPackage generated/ic-ref.nix {};
  leb128-cereal = super.callPackage generated/leb128-cereal.nix {};
  candid = super.callPackage generated/candid.nix {};

  # no base32 in nixos-20.03
  base32 = super.callPackage generated/base32.nix {};

  # need newer version
  base64-bytestring = nix.haskell.lib.dontCheck (super.callPackage generated/base64-bytestring.nix {});
  random = nix.haskell.lib.dontCheck (super.callPackage generated/random.nix {});
  splitmix = nix.haskell.lib.dontCheck (super.callPackage generated/splitmix.nix {});
  QuickCheck = super.callPackage generated/QuickCheck.nix {};
  megaparsec = super.callPackage generated/megaparsec.nix {};

  # Only the test suite of crc is broken
  # https://github.com/MichaelXavier/crc/issues/2
  crc = nix.haskell.lib.markUnbroken (nix.haskell.lib.dontCheck super.crc);

  # We want random-1.2, but a lot of packages do not like that yet,
  # luckily mostly in the test suite. Hence this set of fix-ups:
  # (Remove when going to a nipxkgs that has random-1.2)

  # Wants testing-framework, not compatible with random-1.2
  test-framework-quickcheck2 = nix.haskell.lib.markBroken super.test-framework-quickcheck2;
  Glob = nix.haskell.lib.dontCheck super.Glob;
  SHA = nix.haskell.lib.dontCheck super.SHA;
  blaze-builder = nix.haskell.lib.dontCheck super.blaze-builder;
  blaze-html = nix.haskell.lib.dontCheck super.blaze-html;
  cereal = nix.haskell.lib.dontCheck super.cereal;
  either = nix.haskell.lib.dontCheck super.either;
  exceptions = nix.haskell.lib.dontCheck super.exceptions;
  hashable = nix.haskell.lib.dontCheck super.hashable;
  unordered-containers = nix.haskell.lib.dontCheck super.unordered-containers;
  monad-par = nix.haskell.lib.dontCheck super.monad-par;
  cassava = nix.haskell.lib.dontCheck super.cassava;
  network-uri = nix.haskell.lib.dontCheck super.network-uri;
  pem = nix.haskell.lib.dontCheck super.pem;
  pureMD5 = nix.haskell.lib.dontCheck super.pureMD5;

  # hedgehog wants older random
  hedgehog = nix.haskell.lib.markBroken super.hedgehog;
  bsb-http-chunked = nix.haskell.lib.dontCheck super.bsb-http-chunked;
  retry = nix.haskell.lib.dontCheck super.retry;

  # wants older quickcheck
  quickcheck-instances = nix.haskell.lib.markBroken super.quickcheck-instances;
  aeson = nix.haskell.lib.dontCheck super.aeson;
  prettyprinter = nix.haskell.lib.dontCheck super.prettyprinter;
  http-types = nix.haskell.lib.dontCheck super.http-types;
  vector-builder = nix.haskell.lib.dontCheck super.vector-builder;
  serialise = nix.haskell.lib.dontCheck super.serialise;

  # not compatible with latest quickcheck
  psqueues = nix.haskell.lib.dontCheck super.psqueues;
  vector = nix.haskell.lib.dontCheck super.vector;
  attoparsec = nix.haskell.lib.dontCheck super.attoparsec;
}
