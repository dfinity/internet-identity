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
  /** The successor announced at the last sign-in, kept from before the call until that
   *  sign-in is known to have been accepted. The canister reaches this browser's entry
   *  only through the successor it announced, so losing this key while the canister kept
   *  it would leave the browser unable to prove it is itself ever again. */
  announced?: CryptoKeyPair;
  /** Absent until a sign-in has told us which browser we are. */
  deviceId?: number;
}

/**
 * Thrown by a sign-in the canister refused because this browser's key is one it has
 * already retired, which is what a lost response leaves behind.
 *
 * Raised by the caller that can read the canister's answer; handled here, because this is
 * where the successor that does resolve is kept.
 */
export class StaleBrowserKeyError extends Error {
  constructor() {
    super("the canister has already retired this browser's key");
    this.name = "StaleBrowserKeyError";
  }
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

/** The record a sign-in proves with: what is stored, completed with whatever it lacks. */
const prepared = async (
  identityNumber: bigint,
  from?: BrowserKeyRecord,
): Promise<Required<Pick<BrowserKeyRecord, "keyPair" | "announced">>> => {
  const stored = from ?? (await read(identityNumber));
  const keyPair = stored?.keyPair ?? (await generate());
  // Both halves go on disk before the call. The canister may accept this sign-in and
  // never tell us, and from that moment the only key that reaches our entry is the
  // successor we announced — a successor generated and discarded per attempt would be
  // gone with the response that carried it.
  const announced = stored?.announced ?? (await generate());
  if (stored?.keyPair !== keyPair || stored?.announced !== announced) {
    await write(identityNumber, { ...stored, keyPair, announced });
  }
  return { keyPair, announced };
};

/** One attempt, proving `keyPair` and announcing `announced`. */
const attempt = async <T>(
  identityNumber: bigint,
  sessionKey: Uint8Array,
  signIn: (proof: BrowserProof) => Promise<T>,
  from?: BrowserKeyRecord,
): Promise<T> => {
  const { keyPair, announced: successor } = await prepared(
    identityNumber,
    from,
  );

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
};

/**
 * Proves possession of this browser's key and announces the successor it rotates to.
 *
 * The proof covers the session key, which is fresh for every session, so it is good for
 * exactly one sign-in. `accept` is what advances this browser to the successor.
 *
 * The canister accepts only the successor an entry is waiting for, so a sign-in whose
 * response was lost leaves this browser proving with a key that has since been retired.
 * That is refused rather than registered afresh, and this is the only party holding the
 * key that does resolve: on refusal the announced successor is promoted and the sign-in
 * runs once more. The old key is discarded only after that succeeds.
 */
export const withBrowserProof = <T>(
  identityNumber: bigint,
  sessionKey: Uint8Array,
  signIn: (proof: BrowserProof) => Promise<T>,
): Promise<T> =>
  exclusively(identityNumber, async () => {
    try {
      return await attempt(identityNumber, sessionKey, signIn);
    } catch (error) {
      if (!(error instanceof StaleBrowserKeyError)) {
        throw error;
      }
      const stored = await read(identityNumber);
      // Nothing to promote means the canister holds an entry this browser can no longer
      // reach — only possible where a write was lost, since the successor is stored before
      // it is announced. Starting over costs a second row in the user's list, which beats
      // a browser that can never sign in again.
      const promoted: BrowserKeyRecord = {
        keyPair: stored?.announced ?? (await generate()),
      };
      // Carried into the retry rather than read back, so a storage failure costs the
      // rotation and not the sign-in.
      await write(identityNumber, promoted);
      return await attempt(identityNumber, sessionKey, signIn, promoted);
    }
  });

/** Which browser the canister knows this one as, for the settings list to mark it. */
export const currentDeviceId = async (
  identityNumber: bigint,
): Promise<number | undefined> => (await read(identityNumber))?.deviceId;
