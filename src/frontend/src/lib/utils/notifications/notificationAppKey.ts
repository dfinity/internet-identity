/**
 * The key a Home Screen app carries notifications under.
 *
 * Separate from `browser-key.store.ts`, which holds the key a browser signs in with:
 * that key rotates at every sign-in and is kept per identity, while this one never signs
 * in and never rotates, because the only thing it can do is set and read one push
 * subscription. A Home Screen app also has its own storage partition, so it could not
 * reach the other store even if the semantics matched.
 *
 * Storage only. The service worker reads this to know who it can sign for, and a worker
 * that reached `$lib/globals` from here would pull the page's DOM-bound modules into a
 * context that has no `document`.
 */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

const APP_KEY_STORE = createStore("ii-notification-app", "key");
const RECORD_KEY = "key";

export interface AppKeyRecord {
  keyPair: CryptoKeyPair;
}

/** Which identities this app serves is not kept here: the canister is the only party
 *  that knows, since a browser can point another identity at this app without the app
 *  ever running. The app asks it, every launch. */

/** `undefined` where this app has never run, or where storage refuses to answer. */
export const readAppKey = async (): Promise<AppKeyRecord | undefined> => {
  try {
    return await idbGet<AppKeyRecord>(RECORD_KEY, APP_KEY_STORE);
  } catch {
    return undefined;
  }
};

/**
 * The key this app has, generating one the first time it runs.
 *
 * Non-extractable, like the browser key: it is the app's name to the canister, and a
 * copy taken off disk would be able to read and replace this app's subscription.
 */
export const ensureAppKey = async (): Promise<AppKeyRecord> => {
  const stored = await readAppKey();
  if (stored !== undefined) {
    return stored;
  }
  const keyPair = await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    false,
    ["sign"],
  );
  const record: AppKeyRecord = { keyPair };
  await idbSet(RECORD_KEY, record, APP_KEY_STORE);
  return record;
};

export const appPublicKey = (record: AppKeyRecord): Promise<Uint8Array> =>
  crypto.subtle
    .exportKey("spki", record.keyPair.publicKey)
    .then((spki) => new Uint8Array(spki));
