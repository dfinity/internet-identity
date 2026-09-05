import "fake-indexeddb/auto";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { clear, createStore, set as idbSet } from "idb-keyval";

/** Lets one test refuse a write, which is the only way the store ends up holding a key
 *  without the successor it announced. */
const storage = vi.hoisted(() => ({ writesFail: false }));

vi.mock("idb-keyval", async (importOriginal) => {
  const actual = await importOriginal<typeof import("idb-keyval")>();
  return {
    ...actual,
    set: (...args: Parameters<typeof actual.set>) =>
      storage.writesFail
        ? Promise.reject(new Error("quota exceeded"))
        : actual.set(...args),
  };
});
import {
  currentDeviceId,
  StaleBrowserKeyError,
  withBrowserProof,
} from "./browser-key.store";

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
    storage.writesFail = false;
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
  });

  /// The canister may have accepted the sign-in and never told us, and from then on the
  /// announced successor is the only key that reaches our entry. Announcing a fresh one
  /// instead would leave the entry waiting for a key nobody holds.
  it("re-announces the successor it already announced", async () => {
    const first = await attempt(IDENTITY, 1);

    const second = await attempt(IDENTITY, 2);

    expect(second.nextPublicKey).toEqual(first.nextPublicKey);
  });

  it("promotes the announced successor when the canister calls the key stale", async () => {
    const first = await attempt(IDENTITY, 1);

    let seen = 0;
    const proof = await withBrowserProof(
      IDENTITY,
      sessionKey(2),
      (attempted) => {
        seen += 1;
        if (seen === 1) {
          return Promise.reject(new StaleBrowserKeyError());
        }
        return Promise.resolve(attempted);
      },
    );

    expect(seen).toBe(2);
    expect(proof.publicKey).toEqual(first.nextPublicKey);
  });

  /// Reachable because a write is allowed to fail silently: a browser that could not keep
  /// the successor it announced holds nothing the canister's entry is waiting for. A second
  /// row in the user's list beats a browser that can never sign in again.
  it("starts over when a stale key has no successor to promote", async () => {
    const orphaned = await crypto.subtle.generateKey(
      { name: "ECDSA", namedCurve: "P-256" },
      false,
      ["sign", "verify"],
    );
    await idbSet(IDENTITY.toString(), { keyPair: orphaned }, BROWSER_KEY_STORE);
    const stranded = new Uint8Array(
      await crypto.subtle.exportKey("spki", orphaned.publicKey),
    );
    storage.writesFail = true;

    let seen = 0;
    const proof = await withBrowserProof(
      IDENTITY,
      sessionKey(1),
      (attempted) => {
        seen += 1;
        return seen === 1
          ? Promise.reject(new StaleBrowserKeyError())
          : Promise.resolve(attempted);
      },
    );

    expect(seen).toBe(2);
    expect(proof.publicKey).not.toEqual(stranded);
  });

  it("does not retry a failure that is not a stale key", async () => {
    let seen = 0;

    await expect(
      withBrowserProof(IDENTITY, sessionKey(1), () => {
        seen += 1;
        return Promise.reject(new Error("network"));
      }),
    ).rejects.toThrow("network");
    expect(seen).toBe(1);
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
