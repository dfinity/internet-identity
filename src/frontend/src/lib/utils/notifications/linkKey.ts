/** The one-shot key an install URL carries: the browser mints it and registers its principal, and the Home Screen app spends it on the one call that links itself. */
import { fromBase64URL, toBase64URL } from "$lib/utils/utils";

const LINK_KEY_ALGORITHM = { name: "ECDSA", namedCurve: "P-256" } as const;

const COORDINATE_BYTES = 32;

/** `d`, `x` and `y` of the key's JWK. The point travels with the scalar because WebCrypto cannot recover a public key from a private one. */
export const LINK_KEY_BYTES = 3 * COORDINATE_BYTES;

/** Extractable, unlike the keys this browser keeps: it exists to be carried into the app's own storage partition. */
export const generateLinkKey = (): Promise<CryptoKeyPair> =>
  crypto.subtle.generateKey(LINK_KEY_ALGORITHM, true, [
    "sign",
    "verify",
  ]) as Promise<CryptoKeyPair>;

const coordinate = (value: string | undefined): Uint8Array => {
  if (value === undefined) {
    throw new Error("the link key is missing a coordinate");
  }
  return fromBase64URL(value);
};

export const exportLinkKey = async (
  keyPair: CryptoKeyPair,
): Promise<Uint8Array> => {
  const { d, x, y } = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
  const bytes = new Uint8Array(LINK_KEY_BYTES);
  bytes.set(coordinate(d), 0);
  bytes.set(coordinate(x), COORDINATE_BYTES);
  bytes.set(coordinate(y), 2 * COORDINATE_BYTES);
  return bytes;
};

export const importLinkKey = async (
  linkKey: Uint8Array,
): Promise<CryptoKeyPair> => {
  const [d, x, y] = [0, 1, 2].map((index) =>
    toBase64URL(
      linkKey.subarray(
        index * COORDINATE_BYTES,
        (index + 1) * COORDINATE_BYTES,
      ),
    ),
  );
  const [privateKey, publicKey] = await Promise.all([
    crypto.subtle.importKey(
      "jwk",
      { kty: "EC", crv: "P-256", d, x, y },
      LINK_KEY_ALGORITHM,
      false,
      ["sign"],
    ),
    // Extractable, because an identity built from the pair reads the public key back out as DER.
    crypto.subtle.importKey(
      "jwk",
      { kty: "EC", crv: "P-256", x, y },
      LINK_KEY_ALGORITHM,
      true,
      ["verify"],
    ),
  ]);
  return { privateKey, publicKey };
};
