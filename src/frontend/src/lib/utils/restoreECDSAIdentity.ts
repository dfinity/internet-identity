import { ECDSAKeyIdentity } from "@dfinity/identity";

export const restoreECDSAIdentity = async (jwkPair: {
  privateKey: JsonWebKey;
  publicKey: JsonWebKey;
}): Promise<ECDSAKeyIdentity> => {
  const [privateKey, publicKey] = await Promise.all([
    crypto.subtle.importKey(
      "jwk",
      jwkPair.privateKey,
      { name: "ECDSA", namedCurve: "P-256" },
      false,
      ["sign"],
    ),
    crypto.subtle.importKey(
      "jwk",
      jwkPair.publicKey,
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["verify"],
    ),
  ]);

  return ECDSAKeyIdentity.fromKeyPair({ privateKey, publicKey });
};
