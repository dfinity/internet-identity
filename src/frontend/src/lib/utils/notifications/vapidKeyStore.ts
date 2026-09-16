// The device's VAPID signing key, kept so the JWT pool can be refreshed without
// re-subscribing. The private key is non-extractable but structured-cloneable, so
// IndexedDB holds an opaque handle the page can sign with and its bytes never leave the
// browser. One record per browser.

import {
  createStore,
  del as idbDel,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

const VAPID_KEY_STORE = createStore("ii-notification-vapid-keys", "keys");
const CURRENT_KEY = "current";

export interface StoredVapidKey {
  /** The push endpoint this key was registered for. */
  endpoint: string;
  /** Non-extractable P-256 private key used to sign the JWT pool. */
  privateKey: CryptoKey;
  /** Uncompressed public key (65 bytes), the applicationServerKey. */
  publicKeyRaw: Uint8Array;
}

export const storeVapidKey = (record: StoredVapidKey): Promise<void> =>
  idbSet(CURRENT_KEY, record, VAPID_KEY_STORE);

export const loadVapidKey = (): Promise<StoredVapidKey | undefined> =>
  idbGet(CURRENT_KEY, VAPID_KEY_STORE);

export const purgeVapidKey = (): Promise<void> =>
  idbDel(CURRENT_KEY, VAPID_KEY_STORE);
