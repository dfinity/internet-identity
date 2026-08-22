import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

/**
 * The key this browser proves itself with when it creates a session, and the id the
 * canister attributed it to.
 *
 * The key never leaves this origin: it appears in no delegation chain and in nothing an app
 * receives, which is what lets it identify the browser without letting two apps recognise
 * it. It is replaced at every sign-in, so a copy of it taken off disk stops working as soon
 * as this browser signs in again.
 */
interface BrowserKeyRecord {
  keyPair: CryptoKeyPair;
  /** Absent until a sign-in has told us which browser we are. */
  deviceId?: number;
}

const BROWSER_KEY_STORE = createStore("ii-browser-keys", "keys");

/** Must match the domains the canister verifies the two signatures under. */
const SIGNATURE_DOMAIN = new TextEncoder().encode("ii-session-device-key");
const SUCCESSOR_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-session-device-successor",
);

/**
 * One key per identity, so nothing stored here links two of the user's identities to the
 * same browser.
 */
const storageKey = (identityNumber: bigint): string =>
  identityNumber.toString();

const generate = (): Promise<CryptoKeyPair> =>
  crypto.subtle.generateKey({ name: "ECDSA", namedCurve: "P-256" }, false, [
    "sign",
    "verify",
  ]) as Promise<CryptoKeyPair>;

const read = async (
  identityNumber: bigint,
): Promise<BrowserKeyRecord | undefined> => {
  try {
    return await idbGet<BrowserKeyRecord>(
      storageKey(identityNumber),
      BROWSER_KEY_STORE,
    );
  } catch {
    return undefined;
  }
};

const write = async (
  identityNumber: bigint,
  record: BrowserKeyRecord,
): Promise<void> => {
  try {
    await idbSet(storageKey(identityNumber), record, BROWSER_KEY_STORE);
  } catch {
    // A browser that cannot keep its key signs in as a new one next time, which the
    // identity sees as a new browser rather than as a failure.
  }
};

const exported = (key: CryptoKey): Promise<Uint8Array> =>
  crypto.subtle.exportKey("spki", key).then((spki) => new Uint8Array(spki));

const signed = async (
  key: CryptoKey,
  domain: Uint8Array,
  sessionKey: Uint8Array,
  otherKey: Uint8Array,
): Promise<Uint8Array> => {
  const message = new Uint8Array(
    domain.length + sessionKey.length + otherKey.length,
  );
  message.set(domain);
  message.set(sessionKey, domain.length);
  message.set(otherKey, domain.length + sessionKey.length);
  return new Uint8Array(
    await crypto.subtle.sign({ name: "ECDSA", hash: "SHA-256" }, key, message),
  );
};

export interface BrowserProof {
  publicKey: Uint8Array;
  nextPublicKey: Uint8Array;
  signature: Uint8Array;
  /** By the successor itself, so a key the browser does not hold cannot be announced. */
  nextSignature: Uint8Array;
  /** Rotates to the successor. Called once the canister has accepted the sign-in. */
  accept: (deviceId: number) => Promise<void>;
}

/** Serialises sign-ins for one identity: two at once would leave us holding a key the
 *  canister never accepted, which reads as a different browser. */
const exclusively = async <T>(
  identityNumber: bigint,
  run: () => Promise<T>,
): Promise<T> => {
  const locks = navigator.locks;
  if (locks === undefined) {
    return run();
  }
  // Awaited, because `request` types its callback's return as the value it resolves to,
  // so the promise `run` returns would otherwise nest.
  return await locks.request(`ii-browser-key:${identityNumber}`, run);
};

/**
 * Proves possession of this browser's key and announces the successor it rotates to.
 *
 * The proof covers the session key, which is fresh for every session, so it is good for
 * exactly one sign-in. `accept` is what advances this browser to the successor, and until
 * it is called the current key stays in place — so a call that never comes back leaves both
 * sides on the key the canister still holds.
 */
export const withBrowserProof = <T>(
  identityNumber: bigint,
  sessionKey: Uint8Array,
  signIn: (proof: BrowserProof) => Promise<T>,
): Promise<T> =>
  exclusively(identityNumber, async () => {
    const stored = await read(identityNumber);
    let keyPair = stored?.keyPair;
    if (keyPair === undefined) {
      // Kept before the call, not after: a first sign-in whose response is lost has still
      // registered this key, and coming back with a different one would enrol us twice.
      keyPair = await generate();
      await write(identityNumber, { keyPair });
    }
    const successor = await generate();
    const [publicKey, nextPublicKey] = await Promise.all([
      exported(keyPair.publicKey),
      exported(successor.publicKey),
    ]);

    const [signature, nextSignature] = await Promise.all([
      signed(keyPair.privateKey, SIGNATURE_DOMAIN, sessionKey, nextPublicKey),
      signed(
        successor.privateKey,
        SUCCESSOR_SIGNATURE_DOMAIN,
        sessionKey,
        publicKey,
      ),
    ]);

    return signIn({
      publicKey,
      nextPublicKey,
      signature,
      nextSignature,
      accept: (deviceId) =>
        write(identityNumber, { keyPair: successor, deviceId }),
    });
  });

/** Which browser the canister knows this one as, for the settings list to mark it. */
export const currentDeviceId = async (
  identityNumber: bigint,
): Promise<number | undefined> => (await read(identityNumber))?.deviceId;
