import { ECDSAKeyIdentity } from "@dfinity/identity";

export const restoreECDSAIdentity = async (jwkPair: {
  privateKey: JsonWebKey;
  publicKey: JsonWebKey;
}): Promise<ECDSAKeyIdentity> => {
  if (
    jwkPair === undefined ||
    jwkPair.privateKey === undefined ||
    jwkPair.privateKey.kty === undefined
  ) {
    throw new Error("Invalid privateKey JWK");
  }
  if (jwkPair.publicKey === undefined || jwkPair.publicKey.kty === undefined) {
    throw new Error("Invalid publicKey JWK");
  }

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
