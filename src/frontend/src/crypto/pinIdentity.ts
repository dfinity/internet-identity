import { SignIdentity } from "@dfinity/agent";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { z } from "zod";

/** This module defines the crypto operations for the browser storage Identity.
 *
 * The identity is an ECDSA keypair. The keypair is meant to be indirectly stored in browser storage (IndexedDB).
 * Before being stored, the keypair in encrypted twice (symmetrically).
 *
 * The first encryption round uses an AES-GCM key, which may be stored (unencrypted _but_ unextractable) in the
 * browser.
 * The second encryption round uses an AES-GCM key, derived (PBKDF2) from a user-defined PIN.
 *
 * All the data necessary to recreated & retrieve the keys (salt, initialization vectors, iterations) may also
 * be stored in the browser.
 *
 * The following terms are used:
 *  * construct/reconstruct: how to originally construct and later re-construct the identity keypair
 *  * generate: originally generate either an AES key or ECDSA keypair
 *  * derive: derive a key from e.g. the PIN
 */

export const PinIdentityMaterial = z.object({
  /* The ECDSA keypair */
  publicKey: z.instanceof(CryptoKey),
  encryptedPrivateKey: z.instanceof(ArrayBuffer),
  /* any of the NIST approved curves
   * https://developer.mozilla.org/en-US/docs/Web/API/EcKeyGenParams#instance_properties */
  namedCurve: z.enum(["P-256", "P-384", "P-521"]),

  /* The first round of encryption with browser key */
  browserIv: z.instanceof(Uint8Array),
  browserKey: z.instanceof(CryptoKey),

  /* The second round of encryption with PIN key */
  pinIv: z.instanceof(Uint8Array),
  pinSalt: z.instanceof(Uint8Array),
  pinPbkdfIters: z.number(),
});
export type NistEc = PinIdentityMaterial["namedCurve"];
export type PinIdentityMaterial = z.infer<typeof PinIdentityMaterial>;

export const constructPinIdentity = async ({
  pin,
}: {
  pin: string;
}): Promise<{
  identity: SignIdentity;
  pinIdentityMaterial: PinIdentityMaterial;
}> => {
  // For initialization vectors, recommendation is 96 bit = 12 bytes
  const browserIv = window.crypto.getRandomValues(new Uint8Array(12));
  const pinIv = window.crypto.getRandomValues(new Uint8Array(12));
  // For salt, recommendation is 128 bit = 16 bytes
  const pinSalt = window.crypto.getRandomValues(new Uint8Array(16));
  // Recommended iterations is 600k
  // https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html#pbkdf2
  const pinPbkdfIters: number = 600_1000;
  const keypairNamedCurve: NistEc = "P-256";

  /* The (extractable, for storage) key pair */
  const keypair_ = await generateKeyPair({ namedCurve: keypairNamedCurve });
  const secretKey = keypair_.getKeyPair().privateKey;

  // We re-create a non-extractable

  const browserKey = await generateBrowserKey();
  const encryptedOnce = await crypto.subtle.wrapKey(
    "pkcs8" /* the export format */,
    secretKey /* the key to wrap */,
    browserKey /* the wrapping key */,
    { name: "AES-GCM", iv: browserIv }
  );

  /* Instead of using the extractable key pair as identity, we re-create it
   * from the wrap key but this time NON-extractable */
  const keypairPrivate = await crypto.subtle.unwrapKey(
    "pkcs8",
    encryptedOnce,
    browserKey,
    { name: "AES-GCM", iv: browserIv },
    { name: "ECDSA", namedCurve: keypairNamedCurve },
    false /* non-extractable */,
    ["sign"] /* key usages */
  );
  const keypair = await ECDSAKeyIdentity.fromKeyPair({
    publicKey: keypair_.getKeyPair().publicKey,
    privateKey: keypairPrivate,
  });

  const pinKey = await derivePinKey({
    pin,
    salt: pinSalt,
    iterations: pinPbkdfIters,
  });
  const encryptedTwice = await crypto.subtle.encrypt(
    { name: "AES-GCM", iv: pinIv },
    pinKey,
    encryptedOnce
  );

  return {
    identity: keypair,
    pinIdentityMaterial: {
      publicKey: keypair.getKeyPair().publicKey,
      encryptedPrivateKey: encryptedTwice,
      namedCurve: keypairNamedCurve,

      browserIv,
      browserKey,

      pinIv,
      pinSalt,
      pinPbkdfIters,
    },
  };
};

export const reconstructPinIdentity = async ({
  pin,
  pinIdentityMaterial: {
    encryptedPrivateKey: encryptedTwice,
    publicKey,
    namedCurve,

    browserIv,
    browserKey,

    pinIv,
    pinSalt,
    pinPbkdfIters,
  },
}: {
  pin: string;
  pinIdentityMaterial: PinIdentityMaterial;
}): Promise<SignIdentity> => {
  const pinKey = await derivePinKey({
    pin,
    salt: pinSalt,
    iterations: pinPbkdfIters,
  });
  const encryptedOnce = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv: pinIv },
    pinKey,
    encryptedTwice
  );

  const secretKey = await crypto.subtle.unwrapKey(
    "pkcs8" /* the export format */,
    encryptedOnce,
    browserKey,
    { name: "AES-GCM", iv: browserIv } /* the encryption algo */,
    { name: "ECDSA", namedCurve },
    false /* non-extractable */,
    ["sign"] /* key usages */
  );

  return ECDSAKeyIdentity.fromKeyPair({ privateKey: secretKey, publicKey });
};

/** PIN identity key pair */

// Generates a new ECDSA key pair
export const generateKeyPair = async ({
  namedCurve,
}: {
  namedCurve: NistEc;
}): Promise<ECDSAKeyIdentity> => {
  const keyPair = await crypto.subtle.generateKey(
    {
      name: "ECDSA",
      namedCurve,
    },
    true /* extractable for storage */,
    ["sign"] /* The only usage of the actual keypair is to sign requests */
  );

  return ECDSAKeyIdentity.fromKeyPair(keyPair);
};

/** Encryption keys */

const AesKeyGenParams = { name: "AES-GCM", length: 256 } as const;

/* PIN key */

export const derivePinKey = async ({
  pin,
  salt,
  iterations,
}: {
  pin: string;
  salt: ArrayBuffer;
  iterations: number;
}): Promise<CryptoKey> => {
  const enc = new TextEncoder();

  // https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/deriveKey#pbkdf2_2
  const passwordKeyMaterial = await crypto.subtle.importKey(
    "raw",
    enc.encode(pin),
    "PBKDF2",
    false /* non extractable */,
    ["deriveKey"] /* this material is only used to derive the PIN key */
  );

  const encryptionKey = await crypto.subtle.deriveKey(
    {
      name: "PBKDF2",
      salt,
      iterations,
      hash: "SHA-256",
    },
    passwordKeyMaterial,
    AesKeyGenParams,
    false /* non-extractable, only used to encrypt/decrypt */,
    [
      "encrypt",
      "decrypt",
    ] /* The password-derived key is used to encrypt/decrypt the keypair (2nd encryption round )*/
  );

  return encryptionKey;
};

/* Browser key */

export const generateBrowserKey = async (): Promise<CryptoKey> => {
  const key = await crypto.subtle.generateKey(
    AesKeyGenParams,
    false /* non extractable */,
    [
      "wrapKey",
      "unwrapKey",
    ] /* key is used to wrap (and unwrap) the identity secret key */
  );
  return key;
};
