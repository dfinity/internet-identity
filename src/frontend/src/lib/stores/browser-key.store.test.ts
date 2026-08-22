import "fake-indexeddb/auto";
import { beforeEach, describe, expect, it } from "vitest";
import { clear, createStore } from "idb-keyval";
import { currentDeviceId, withBrowserProof } from "./browser-key.store";

/// Names the same store the module under test writes to, so a test can wipe it.
const BROWSER_KEY_STORE = createStore("ii-browser-keys", "keys");

const SIGNATURE_DOMAIN = new TextEncoder().encode("ii-session-device-key");
const SUCCESSOR_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-session-device-successor",
);

const signedMessage = (
  domain: Uint8Array,
  sessionKey: Uint8Array,
  otherKey: Uint8Array,
): Uint8Array => {
  const message = new Uint8Array(
    domain.length + sessionKey.length + otherKey.length,
  );
  message.set(domain);
  message.set(sessionKey, domain.length);
  message.set(otherKey, domain.length + sessionKey.length);
  return message;
};

const verify = async (
  publicKey: Uint8Array,
  signature: Uint8Array,
  message: Uint8Array,
): Promise<boolean> => {
  const key = await crypto.subtle.importKey(
    "spki",
    new Uint8Array(publicKey),
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["verify"],
  );
  return crypto.subtle.verify(
    { name: "ECDSA", hash: "SHA-256" },
    key,
    new Uint8Array(signature),
    new Uint8Array(message),
  );
};

const sessionKey = (seed: number) => new Uint8Array(62).fill(seed);

const IDENTITY = BigInt(10_000);

/** Signs in and rotates, the way a successful ceremony does. */
const signIn = (identityNumber: bigint, seed: number, deviceId = 1) =>
  withBrowserProof(identityNumber, sessionKey(seed), async (proof) => {
    await proof.accept(deviceId);
    return proof;
  });

/** Signs in without accepting, the way a call that fails or never returns leaves it. */
const attempt = (identityNumber: bigint, seed: number) =>
  withBrowserProof(identityNumber, sessionKey(seed), (proof) =>
    Promise.resolve(proof),
  );

/** jsdom has no Web Locks, so this is what serialisation is tested against. */
const stubLockApi = (): void => {
  let tail: Promise<unknown> = Promise.resolve();
  Object.defineProperty(navigator, "locks", {
    configurable: true,
    value: {
      request: (_name: string, run: () => Promise<unknown>) => {
        const next = tail.then(run);
        tail = next.then(
          () => undefined,
          () => undefined,
        );
        return next;
      },
    },
  });
};

const withoutLockApi = (): void => {
  Object.defineProperty(navigator, "locks", {
    configurable: true,
    value: undefined,
  });
};

describe("browser key", () => {
  beforeEach(async () => {
    await clear(BROWSER_KEY_STORE);
    withoutLockApi();
  });

  it("signs the session key and the successor under the domain the canister verifies", async () => {
    const key = sessionKey(1);

    const proof = await attempt(IDENTITY, 1);

    await expect(
      verify(
        proof.publicKey,
        proof.signature,
        signedMessage(SIGNATURE_DOMAIN, key, proof.nextPublicKey),
      ),
    ).resolves.toBe(true);
  });

  it("does not sign the session key alone", async () => {
    const proof = await attempt(IDENTITY, 1);

    await expect(
      verify(proof.publicKey, proof.signature, sessionKey(1)),
    ).resolves.toBe(false);
  });

  it("announces a successor it does not yet use", async () => {
    const proof = await attempt(IDENTITY, 1);

    expect(proof.nextPublicKey).not.toEqual(proof.publicKey);
  });

  it("rotates to the successor once a sign-in is accepted", async () => {
    const first = await signIn(IDENTITY, 1);

    const second = await attempt(IDENTITY, 2);

    expect(second.publicKey).toEqual(first.nextPublicKey);
  });

  it("keeps the current key when a sign-in is not accepted", async () => {
    const first = await attempt(IDENTITY, 1);

    const second = await attempt(IDENTITY, 2);

    expect(second.publicKey).toEqual(first.publicKey);
    expect(second.nextPublicKey).not.toEqual(first.nextPublicKey);
  });

  it("holds a separate key per identity", async () => {
    const first = await attempt(IDENTITY, 1);

    const second = await attempt(BigInt(10_001), 1);

    expect(second.publicKey).not.toEqual(first.publicKey);
  });

  it("registers a fresh key once storage is cleared", async () => {
    const before = await signIn(IDENTITY, 1);
    await clear(BROWSER_KEY_STORE);

    const after = await attempt(IDENTITY, 1);

    expect(after.publicKey).not.toEqual(before.publicKey);
    expect(after.publicKey).not.toEqual(before.nextPublicKey);
  });

  it("serialises concurrent sign-ins, so the second builds on the first", async () => {
    stubLockApi();

    const [first, second] = await Promise.all([
      signIn(IDENTITY, 1),
      signIn(IDENTITY, 2),
    ]);

    expect(second.publicKey).toEqual(first.nextPublicKey);
  });

  it("still signs in on a browser without the lock API", async () => {
    const proof = await attempt(IDENTITY, 1);

    expect(proof.publicKey.length).toBe(91);
  });

  it("exports the keys in the encoding the canister parses", async () => {
    const proof = await attempt(IDENTITY, 1);

    expect(proof.publicKey.length).toBe(91);
    expect(proof.nextPublicKey.length).toBe(91);
    expect(proof.signature.length).toBe(64);
  });

  it("remembers which browser the canister said this is", async () => {
    await signIn(IDENTITY, 1, 7);

    await expect(currentDeviceId(IDENTITY)).resolves.toBe(7);
  });

  it("knows of no browser before a sign-in is accepted", async () => {
    await attempt(IDENTITY, 1);

    await expect(currentDeviceId(IDENTITY)).resolves.toBeUndefined();
  });

  it("has the successor sign for itself, so an unheld key cannot be announced", async () => {
    const key = sessionKey(1);

    const proof = await attempt(IDENTITY, 1);

    await expect(
      verify(
        proof.nextPublicKey,
        proof.nextSignature,
        signedMessage(SUCCESSOR_SIGNATURE_DOMAIN, key, proof.publicKey),
      ),
    ).resolves.toBe(true);
  });

  it("keeps the two signatures in their own roles", async () => {
    const key = sessionKey(1);

    const proof = await attempt(IDENTITY, 1);

    // The successor's signature must not verify as the current key's, or one could be
    // replayed as the other.
    await expect(
      verify(
        proof.publicKey,
        proof.nextSignature,
        signedMessage(SIGNATURE_DOMAIN, key, proof.nextPublicKey),
      ),
    ).resolves.toBe(false);
  });
});
