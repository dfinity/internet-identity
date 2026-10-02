/**
 * The key a Home Screen app carries notifications under, and the entry it claimed.
 *
 * Separate from `browser-key.store.ts`, which holds the key a browser signs in with:
 * that key rotates at every sign-in and is kept per identity, while this one never signs
 * in and never rotates, because the only thing it can do is set and read one push
 * subscription. A Home Screen app also has its own storage partition, so it could not
 * reach the other store even if the semantics matched.
 */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Actor, HttpAgent, type ActorSubclass } from "@icp-sdk/core/agent";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { agentOptions, canisterId } from "$lib/globals";

const APP_KEY_STORE = createStore("ii-notification-app", "key");
const RECORD_KEY = "key";

interface AppKeyRecord {
  keyPair: CryptoKeyPair;
  /** The identity this app was linked to, once the canister has accepted it. Absent
   *  until then, which is what tells a generated key from a linked one. */
  identityNumber?: bigint;
}

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

/** Records which identity accepted this app, so a later launch knows it is linked. */
export const rememberLinked = async (
  record: AppKeyRecord,
  identityNumber: bigint,
): Promise<void> => {
  await idbSet(RECORD_KEY, { ...record, identityNumber }, APP_KEY_STORE);
};

export const appPublicKey = (record: AppKeyRecord): Promise<Uint8Array> =>
  crypto.subtle
    .exportKey("spki", record.keyPair.publicKey)
    .then((spki) => new Uint8Array(spki));

/** An actor signing as this app, which is how the canister knows whose rows these are. */
export const appKeyActor = async (
  record: AppKeyRecord,
): Promise<ActorSubclass<_SERVICE>> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
  return Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: HttpAgent.createSync({ ...agentOptions, identity }),
    canisterId,
  });
};
