/** This key never signs in and never rotates, unlike the one in `browser-key.store.ts`, and a Home Screen app's storage partition could not reach that store anyway.
 * Storage only: a worker reaching `$lib/globals` from here would pull the page's DOM-bound modules into a context that has no `document`. */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

/** Exported so a test wipes the store this writes to rather than naming it a second time. */
export const APP_KEY_STORE = createStore("ii-notification-app", "key");
const RECORD_KEY = "key";

export interface AppKeyRecord {
  keyPair: CryptoKeyPair;
}

/** `unreadable` is not `no-key`: this key is the app's only name to the canister, so each caller has to say what it does when it cannot tell. */
export type StoredAppKey =
  | { kind: "key"; record: AppKeyRecord }
  | { kind: "no-key" }
  | { kind: "unreadable"; reason: string };

export const readAppKey = async (): Promise<StoredAppKey> => {
  try {
    const record = await idbGet<AppKeyRecord>(RECORD_KEY, APP_KEY_STORE);
    return record === undefined ? { kind: "no-key" } : { kind: "key", record };
  } catch (error) {
    return {
      kind: "unreadable",
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};

/** Non-extractable: it is the app's name to the canister, and a copy taken off disk could read and replace this app's subscription. */
export const ensureAppKey = async (): Promise<AppKeyRecord> => {
  const stored = await readAppKey();
  switch (stored.kind) {
    case "key":
      return stored.record;
    case "unreadable":
      // Minting here would give this app a second permanent name while the canister's links still point at the first, which only a reinstall recovers.
      throw new Error(`the app key could not be read: ${stored.reason}`);
    case "no-key": {
      const keyPair = await crypto.subtle.generateKey(
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["sign"],
      );
      const record: AppKeyRecord = { keyPair };
      await idbSet(RECORD_KEY, record, APP_KEY_STORE);
      return record;
    }
  }
};

export const appPublicKey = (record: AppKeyRecord): Promise<Uint8Array> =>
  crypto.subtle
    .exportKey("spki", record.keyPair.publicKey)
    .then((spki) => new Uint8Array(spki));
