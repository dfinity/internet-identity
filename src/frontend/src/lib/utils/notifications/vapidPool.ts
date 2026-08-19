// The device half of the VAPID pool: generate a P-256 keypair, sign one JWT
// per validity window, and hand II only the raw signatures. II holds no VAPID
// key; it reassembles each compact JWT from a signature at send time (see the
// canister's vapid_jwt module). The header bytes, the claim order, and
// WINDOW_NS are a wire contract with that module — change one side only and
// every signature stops verifying at the relay.

import { bufFromBufLike, toBase64URL } from "$lib/utils/utils";

const WINDOW_NS = BigInt(24 * 60 * 60) * BigInt(1_000_000_000); // 24h
const HEADER_B64 = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9"; // {"typ":"JWT","alg":"ES256"}
// Must match the canister's `vapid_jwt::VAPID_SUBJECT`: it is part of the signed
// payload, so a mismatch makes every signature fail at the relay.
const VAPID_SUBJECT = "https://id.ai";

/** One JWT per window; matches the canister's MAX_JWT_POOL_LEN (~30 days). */
export const JWT_POOL_SIZE = 30;

const utf8 = (value: string): Uint8Array => new TextEncoder().encode(value);

export interface VapidKeypair {
  /** Uncompressed P-256 public key (65 bytes): both the applicationServerKey and what II stores. */
  publicKeyRaw: Uint8Array;
  /** Non-extractable private key, kept only long enough to sign the pool. */
  privateKey: CryptoKey;
}

/**
 * Generates a fresh VAPID keypair. The public key is needed to subscribe (which
 * yields the relay endpoint), and only then can the pool be signed — hence the
 * split from {@link signJwtPool}.
 */
export const generateVapidKeypair = async (): Promise<VapidKeypair> => {
  const keyPair = (await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    false,
    ["sign"],
  )) as CryptoKeyPair;
  return {
    publicKeyRaw: new Uint8Array(
      await crypto.subtle.exportKey("raw", keyPair.publicKey),
    ),
    privateKey: keyPair.privateKey,
  };
};

/** Signs the full pool of JWTs covering `relayOrigin`, one per window. */
export const signJwtPool = async (
  privateKey: CryptoKey,
  relayOrigin: string,
  issuedAtNs: bigint,
): Promise<Uint8Array[]> => {
  const signatures: Uint8Array[] = [];
  for (let window = 0; window < JWT_POOL_SIZE; window++) {
    signatures.push(
      await signWindow(privateKey, relayOrigin, issuedAtNs, window),
    );
  }
  return signatures;
};

/** Signs the JWT for a single window. */
const signWindow = async (
  privateKey: CryptoKey,
  relayOrigin: string,
  issuedAtNs: bigint,
  window: number,
): Promise<Uint8Array> => {
  const signingInput = utf8(
    `${HEADER_B64}.${windowPayloadB64(relayOrigin, issuedAtNs, window)}`,
  );
  const signature = await crypto.subtle.sign(
    { name: "ECDSA", hash: "SHA-256" },
    privateKey,
    bufFromBufLike(signingInput),
  );
  return new Uint8Array(signature);
};

/**
 * base64url of the JWT claims for one window — the exact bytes the canister
 * reassembles and the relay verifies. `exp = (issued_at + (window+1)*24h)` in
 * seconds; keys are ordered aud, exp, sub.
 */
export const windowPayloadB64 = (
  relayOrigin: string,
  issuedAtNs: bigint,
  window: number,
): string => {
  const expSecs =
    (issuedAtNs + BigInt(window + 1) * WINDOW_NS) / BigInt(1_000_000_000);
  const payload = `{"aud":${JSON.stringify(relayOrigin)},"exp":${expSecs},"sub":"${VAPID_SUBJECT}"}`;
  return toBase64URL(utf8(payload));
};
