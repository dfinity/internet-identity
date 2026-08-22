import {
  createStore,
  del as idbDel,
  entries as idbEntries,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

/**
 * A delegation for one app, held by this frontend so the app can be signed in
 * again without a passkey ceremony.
 *
 * `chainJson` is canister-signed and delegates to `keyPair`, which is
 * non-extractable and never leaves IndexedDB. Neither half is usable alone: the
 * chain reaches the certified state of the subnet and so must be inert on its
 * own (the same reason the ICRC-167 transport delegates to an ephemeral key
 * rather than the app's), and the key pair signs nothing the chain does not
 * already authorize.
 *
 * A record therefore grants exactly one capability: extending its own chain to
 * an app-supplied session key, for the origin and account it was minted for,
 * until `expiresAtMillis`. That is the same window the app's own copy of the
 * delegation is valid for, so holding it lets an attacker with this browser
 * profile do nothing they could not already do.
 */
export interface AppDelegationRecord {
  /** The principal the app sees, derived from the chain's public key. Identifies
   *  a record within its origin, and is what `?hint=` carries. */
  principal: string;
  identityNumber: bigint;
  accountNumber?: bigint;
  keyPair: CryptoKeyPair;
  chainJson: string;
  expiresAtMillis: number;
}

// Keyed by effective origin, because that is how every request reads it: the
// delegation handler asks "what do I hold for this origin?" on each authorize,
// and a `?hint=` only picks between the answers. `idb-keyval` has no secondary
// indexes, so anything not in the key costs a full scan — which is why the
// identity purge below is the one operation that pays for one, on sign-out
// rather than on sign-in.
//
// An origin holds more than one record only when the user has signed in there
// under more than one identity or account, so these lists are short.
const APP_DELEGATION_STORE = createStore("ii-app-delegations", "origins");

// Treat the last 5 minutes of a delegation's lifetime as already expired, for
// the same reason the session delegation store does: a record that is "valid"
// at this check can still expire between here and IC validation (network
// latency, ingress queue, clock skew). Better to fall through to a ceremony
// than to hand an app a delegation that dies on its first call.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

const isUsable = (record: AppDelegationRecord): boolean =>
  record.expiresAtMillis - EXPIRY_MARGIN_MS > Date.now();

const read = async (origin: string): Promise<AppDelegationRecord[]> => {
  try {
    return (
      (await idbGet<AppDelegationRecord[]>(origin, APP_DELEGATION_STORE)) ?? []
    );
  } catch {
    return [];
  }
};

/** Writes an origin's records, or removes the entry when none are left, so an
 *  emptied origin leaves nothing behind. */
const write = async (
  origin: string,
  records: AppDelegationRecord[],
): Promise<void> => {
  try {
    if (records.length === 0) {
      await idbDel(origin, APP_DELEGATION_STORE);
      return;
    }
    await idbSet(origin, records, APP_DELEGATION_STORE);
  } catch {
    // Nothing here is load-bearing: a record that cannot be written costs a
    // passkey next time, and one that cannot be removed is still ignored by
    // every read that finds it expired.
  }
};

/**
 * Stores a record, replacing any earlier one for the same principal.
 *
 * Best effort: a failure (private browsing, quota, a browser that refuses to
 * structured-clone a `CryptoKeyPair`) costs the user nothing beyond a passkey
 * next time, so it must never fail the sign-in it was minted during.
 */
export const storeAppDelegation = async (
  origin: string,
  record: AppDelegationRecord,
): Promise<void> => {
  const others = (await read(origin)).filter(
    (existing) => existing.principal !== record.principal && isUsable(existing),
  );
  await write(origin, [...others, record]);
};

/** Every usable record for an origin, expired ones dropped on the way past. */
export const appDelegationsForOrigin = async (
  origin: string,
): Promise<AppDelegationRecord[]> => {
  const records = await read(origin);
  const usable = records.filter(isUsable);
  if (usable.length !== records.length) {
    await write(origin, usable);
  }
  return usable;
};

/** Drops one record, for when it turns out not to have survived storage. */
export const discardAppDelegation = async (
  origin: string,
  principal: string,
): Promise<void> => {
  const records = await read(origin);
  await write(
    origin,
    records.filter((record) => record.principal !== principal),
  );
};

/**
 * Forgets every record for an origin, so the next request there needs a
 * ceremony.
 *
 * Wipes the whole origin rather than one principal because a user is only ever
 * signed in to an app as one account at a time, so the others are stale by
 * definition. Nothing here touches the delegation the app itself is holding,
 * which stays valid until it expires and is the app's own to clear.
 */
export const forgetAppDelegations = async (origin: string): Promise<void> => {
  try {
    await idbDel(origin, APP_DELEGATION_STORE);
  } catch {
    // Left for the expiry check to deal with.
  }
};

/**
 * Forgets every record belonging to an identity, across all origins. Called
 * wherever an identity is signed out of or removed from this device.
 *
 * The only operation that scans, because an identity spans origins and the
 * origin is the key. It runs on sign-out, never on sign-in.
 */
export const purgeAppDelegations = async (
  identityNumber: bigint,
): Promise<void> => {
  let stored: [string, AppDelegationRecord[]][];
  try {
    stored = await idbEntries<string, AppDelegationRecord[]>(
      APP_DELEGATION_STORE,
    );
  } catch {
    return;
  }
  await Promise.all(
    stored
      .filter(([, records]) =>
        records.some((record) => record.identityNumber === identityNumber),
      )
      .map(([origin, records]) =>
        write(
          origin,
          records.filter((record) => record.identityNumber !== identityNumber),
        ),
      ),
  );
};
