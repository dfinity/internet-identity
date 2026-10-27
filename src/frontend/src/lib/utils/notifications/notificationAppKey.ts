/** This key never signs in and never rotates, unlike the one in `browser-key.store.ts`, and a Home Screen app's storage partition could not reach that store anyway.
 * Storage only: a worker reaching `$lib/globals` from here would pull the page's DOM-bound modules into a context that has no `document`. */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";
import { Principal } from "@icp-sdk/core/principal";

/** Exported so a test wipes the store this writes to rather than naming it a second time. */
export const APP_KEY_STORE = createStore("ii-notification-app", "key");
const RECORD_KEY = "key";

export interface AppKeyRecord {
  keyPair: CryptoKeyPair;
  /** The record the canister gave this app, absent until it has linked once. Naming it on
   *  a later link joins the record it already has rather than taking a second one. */
  appId?: bigint;
  /** The fragment this app last presented to the canister, token and all. iOS replays
   *  the URL captured at install on every launch, so the same token arrives again and
   *  again; one that is already recorded here has been answered and is not sent twice. */
  spentFragment?: string;
  /** Written by the app's own page when it is running as the installed Home Screen
   *  app. A key's presence cannot stand in for this: a browser that holds one is
   *  still a browser, and only the installed app needs a link handed to Safari. */
  installed?: boolean;
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

/** Records that this storage belongs to the installed app, which only its own page knows. */
export const rememberInstalled = async (): Promise<void> => {
  try {
    const stored = await readAppKey();
    if (stored.kind !== "key" || stored.record.installed === true) {
      return;
    }
    await idbSet(
      RECORD_KEY,
      { ...stored.record, installed: true },
      APP_KEY_STORE,
    );
  } catch (error) {
    console.error(error);
  }
};

/**
 * Remembers the fragment just presented, so a launch replaying it sends nothing.
 *
 * Read back rather than written from the caller's copy: a link running in between was
 * handed that same copy and has already written an `appId` into the stored one, which
 * spreading a stale record would take away again.
 */
export const rememberSpentFragment = async (
  fragment: string,
): Promise<void> => {
  try {
    const stored = await readAppKey();
    if (stored.kind !== "key") {
      return;
    }
    await idbSet(
      RECORD_KEY,
      { ...stored.record, spentFragment: fragment },
      APP_KEY_STORE,
    );
  } catch (error) {
    console.error(error);
  }
};

export const appPublicKey = (record: AppKeyRecord): Promise<Uint8Array> =>
  crypto.subtle
    .exportKey("spki", record.keyPair.publicKey)
    .then((spki) => new Uint8Array(spki));

/** What the canister knows this app as, and what it matches an app-facing caller against. */
export const appPrincipal = async (record: AppKeyRecord): Promise<Principal> =>
  Principal.selfAuthenticating(await appPublicKey(record));

/** Keeps the record the canister allocated, so a later link names it. A failure here costs
 *  this app the record on its next link, never the link it has just completed. */
export const rememberAppId = async (
  record: AppKeyRecord,
  appId: bigint,
): Promise<void> => {
  try {
    await idbSet(RECORD_KEY, { ...record, appId }, APP_KEY_STORE);
  } catch (error) {
    console.error(error);
  }
};
