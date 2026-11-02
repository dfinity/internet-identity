// Encrypting this subscription's `auth` secret to the key the canister minted for this
// subscribe.
//
// RFC 8291 hands `auth` to the application server over TLS. A call to a canister has no
// such channel: it passes a boundary node and reaches every replica, and `(endpoint,
// p256dh, auth)` is all a push the browser will decrypt needs. So `auth` is encrypted to
// a key that exists only inside the canister and only until this subscribe lands.
//
// Built from ECDH, HKDF and AES-GCM rather than an asymmetric cipher because WebCrypto
// has exactly one of those and it is RSA-OAEP, whose keypair a canister cannot mint per
// attempt. These three are the same pieces RFC 8291 itself uses, with the browser as the
// sender this time.

/** A view's own bytes, which is what WebCrypto's parameters are typed to take. */
const bytesOf = (view: Uint8Array): ArrayBuffer =>
  view.buffer.slice(
    view.byteOffset,
    view.byteOffset + view.byteLength,
  ) as ArrayBuffer;

/** Binds both public keys into the derivation, as RFC 8291 §3.4 does with its own. */
const INFO_PREFIX = new TextEncoder().encode("II webpush transport\0");
const KEY_LABEL = new TextEncoder().encode("key\0");
const IV_LABEL = new TextEncoder().encode("iv\0");

/** HKDF with no salt, which is the zero-filled one RFC 5869 §2.2 specifies. */
const NO_SALT = new Uint8Array(0);

const concat = (...parts: Uint8Array[]): Uint8Array => {
  const out = new Uint8Array(parts.reduce((len, part) => len + part.length, 0));
  let at = 0;
  for (const part of parts) {
    out.set(part, at);
    at += part.length;
  }
  return out;
};

export interface SealedSubscription {
  /** The subscription's own public key, which the canister runs every refresh against. */
  p256dh: Uint8Array;
  /** The public half of the pair minted here, which the canister needs to open the rest. */
  transportKey: Uint8Array;
  /** `auth` under AES-128-GCM, 32 bytes with its tag. */
  sealedAuth: Uint8Array;
}

/** Encrypts `subscription`'s `auth` secret to `transportPublicKey`. */
export const sealSubscriptionAuth = async (
  subscription: PushSubscription,
  transportPublicKey: Uint8Array,
): Promise<SealedSubscription> => {
  const subscriberKey = subscription.getKey("p256dh");
  const authSecret = subscription.getKey("auth");
  if (subscriberKey === null || authSecret === null) {
    throw new Error("the push subscription carries no encryption keys");
  }

  // Minted for this one encryption, and non-extractable: nothing here needs it back.
  const browser = await crypto.subtle.generateKey(
    { name: "ECDH", namedCurve: "P-256" },
    false,
    ["deriveBits"],
  );
  const transportKey = new Uint8Array(
    await crypto.subtle.exportKey("raw", browser.publicKey),
  );
  const theirs = await crypto.subtle.importKey(
    "raw",
    bytesOf(transportPublicKey),
    { name: "ECDH", namedCurve: "P-256" },
    false,
    [],
  );
  const shared = await crypto.subtle.deriveBits(
    { name: "ECDH", public: theirs },
    browser.privateKey,
    256,
  );
  const extracted = await crypto.subtle.importKey(
    "raw",
    shared,
    "HKDF",
    false,
    ["deriveBits"],
  );

  // The canister's key first and this one second, which is the order it rebuilds in.
  const info = concat(INFO_PREFIX, transportPublicKey, transportKey);
  const derive = async (
    label: Uint8Array,
    bits: number,
  ): Promise<ArrayBuffer> =>
    crypto.subtle.deriveBits(
      {
        name: "HKDF",
        hash: "SHA-256",
        salt: bytesOf(NO_SALT),
        info: bytesOf(concat(info, label)),
      },
      extracted,
      bits,
    );

  const key = await crypto.subtle.importKey(
    "raw",
    await derive(KEY_LABEL, 128),
    "AES-GCM",
    false,
    ["encrypt"],
  );
  const sealedAuth = new Uint8Array(
    await crypto.subtle.encrypt(
      { name: "AES-GCM", iv: await derive(IV_LABEL, 96) },
      key,
      authSecret,
    ),
  );

  return {
    p256dh: new Uint8Array(subscriberKey),
    transportKey,
    sealedAuth,
  };
};
