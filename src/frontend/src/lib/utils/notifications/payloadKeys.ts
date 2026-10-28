// What a wake-up's payload is sealed with. RFC 8291 derives it from the subscription's
// own keys and an ephemeral pair the sender mints, and the ECDH is the costly half; it
// runs here, once per registration, so the canister only ever expands and encrypts.
//
// A fresh ephemeral pair per upload means the material a registration is sealed under is
// never the material an earlier one used.

import { bufFromBufLike } from "$lib/utils/utils";
import { currentDeviceSubscription } from "./pushSubscription";

/** RFC 8291 §3.4: `"WebPush: info"`, a zero octet, then the two public keys. */
const INFO_PREFIX = new TextEncoder().encode("WebPush: info\0");

export interface PayloadKeys {
  /** The input keying material, 32 bytes. */
  payloadIkm: Uint8Array;
  /** The ephemeral public key the canister echoes as the payload's `keyid`, 65 bytes. */
  payloadKeyid: Uint8Array;
}

const concat = (...parts: Uint8Array[]): Uint8Array => {
  const out = new Uint8Array(parts.reduce((len, part) => len + part.length, 0));
  let at = 0;
  for (const part of parts) {
    out.set(part, at);
    at += part.length;
  }
  return out;
};

/** Derives what `subscription`'s payloads are sealed with. */
export const derivePayloadKeys = async (
  subscription: PushSubscription,
): Promise<PayloadKeys> => {
  const subscriberKey = subscription.getKey("p256dh");
  const authSecret = subscription.getKey("auth");
  if (subscriberKey === null || authSecret === null) {
    throw new Error("the push subscription carries no encryption keys");
  }
  const subscriber = new Uint8Array(subscriberKey);

  const ephemeral = await crypto.subtle.generateKey(
    { name: "ECDH", namedCurve: "P-256" },
    true,
    ["deriveBits"],
  );
  const payloadKeyid = new Uint8Array(
    await crypto.subtle.exportKey("raw", ephemeral.publicKey),
  );
  const theirs = await crypto.subtle.importKey(
    "raw",
    bufFromBufLike(subscriber),
    { name: "ECDH", namedCurve: "P-256" },
    false,
    [],
  );
  const shared = await crypto.subtle.deriveBits(
    { name: "ECDH", public: theirs },
    ephemeral.privateKey,
    256,
  );

  // The auth secret is the salt of this step, and the message salt only enters the
  // expansion the canister does.
  const extracted = await crypto.subtle.importKey(
    "raw",
    shared,
    "HKDF",
    false,
    ["deriveBits"],
  );
  const payloadIkm = new Uint8Array(
    await crypto.subtle.deriveBits(
      {
        name: "HKDF",
        hash: "SHA-256",
        salt: authSecret,
        info: bufFromBufLike(concat(INFO_PREFIX, subscriber, payloadKeyid)),
      },
      extracted,
      256,
    ),
  );
  return { payloadIkm, payloadKeyid };
};

/** Derives them for the subscription this browser holds now. */
export const payloadKeysForThisBrowser = async (): Promise<PayloadKeys> => {
  const subscription = await currentDeviceSubscription();
  if (subscription === undefined) {
    throw new Error("this browser holds no push subscription");
  }
  return derivePayloadKeys(subscription);
};
