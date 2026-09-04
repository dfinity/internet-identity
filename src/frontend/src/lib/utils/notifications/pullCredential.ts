// The credential the service worker uses to pull a notification's content from
// the dApp. It is the user's ordinary per-app identity — a session key plus an
// II-signed account delegation scoped to the dApp's origin — persisted to
// IndexedDB (which a service worker can read, unlike localStorage). Interim
// mechanism: when revocable app sessions land, this is replaced wholesale by a
// revocable session under an AttributesIdentity wrapper.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import { toPermissionsArg } from "$lib/utils/accessLevel";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import {
  createStore,
  del as idbDel,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

const CREDENTIAL_STORE = createStore("ii-notification-credentials", "keys");
// The canister to pull from is a lookup off the origin's well-known, not a trust
// anchor, so it's cached separately and refreshed on a TTL / on pull failure —
// a canister-id change then self-heals without re-consent.
const CANISTER_STORE = createStore("ii-notification-canisters", "keys");
// Notifications the user has already dismissed on this device, so a pull that
// still finds them pending (the dApp evicts on its own cycle) does not re-raise
// them. Keyed by origin, holding the dismissed notifications' tags.
const DISMISSED_STORE = createStore("ii-notification-dismissed", "keys");

export interface NotificationCredentialRecord {
  /** dApp origin, keyed on and also the routing target the SW pulls for. */
  origin: string;
  /** IC host the agent talks to, captured here because the SW can't read globals. */
  host: string;
  keyPair: CryptoKeyPair;
  chainJson: string;
  expiresAtMillis: number;
}

/**
 * Mints a scoped account delegation to a fresh service-worker session key. The
 * delegation is the user's per-app principal at `origin`, so the SW pulls as the
 * same identity the dApp knows.
 */
export const mintNotificationCredential = async ({
  identityNumber,
  accountNumber,
  origin,
  host,
  actor,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
  host: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<NotificationCredentialRecord> => {
  const sessionIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const sessionKey = new Uint8Array(sessionIdentity.getPublicKey().toDer());
  const account: [] | [bigint] =
    accountNumber !== undefined ? [accountNumber] : [];
  const permissions = toPermissionsArg("full-access");

  const { user_key, expiration } = await actor
    .prepare_account_delegation(
      identityNumber,
      origin,
      account,
      sessionKey,
      [],
      permissions,
    )
    .then(throwCanisterError);

  const chain = await actor
    .get_account_delegation(
      identityNumber,
      origin,
      account,
      sessionKey,
      expiration,
      permissions,
    )
    .then(throwCanisterError)
    .then(transformSignedDelegation)
    .then((delegation) =>
      DelegationChain.fromDelegations([delegation], new Uint8Array(user_key)),
    );

  return {
    origin,
    host,
    keyPair: sessionIdentity.getKeyPair(),
    chainJson: JSON.stringify(chain.toJSON()),
    expiresAtMillis: Number(expiration / BigInt(1_000_000)),
  };
};

export const storeNotificationCredential = (
  record: NotificationCredentialRecord,
): Promise<void> => idbSet(record.origin, record, CREDENTIAL_STORE);

export const purgeNotificationCredential = async (
  origin: string,
): Promise<void> => {
  await idbDel(origin, CREDENTIAL_STORE);
  await idbDel(origin, CANISTER_STORE);
  await idbDel(origin, DISMISSED_STORE);
};

/** Tags the user has dismissed for `origin`, so they are not re-raised. */
export const loadDismissed = async (origin: string): Promise<string[]> =>
  (await idbGet(origin, DISMISSED_STORE)) ?? [];

/** Records that the user dismissed `tag` for `origin`. */
export const addDismissed = async (
  origin: string,
  tag: string,
): Promise<void> => {
  const tags = await loadDismissed(origin);
  if (!tags.includes(tag)) {
    await idbSet(origin, [...tags, tag], DISMISSED_STORE);
  }
};

/** Replaces the dismissed set for `origin`, e.g. to drop tags no longer pending. */
export const setDismissed = (origin: string, tags: string[]): Promise<void> =>
  idbSet(origin, tags, DISMISSED_STORE);

export interface CachedCanisters {
  /** Every sender the origin's well-known named, not just the first. */
  canisterIds: string[];
  resolvedAtMillis: number;
}

/** The SW's cached `origin -> senders` resolution, if any. */
export const loadCachedCanisters = (
  origin: string,
): Promise<CachedCanisters | undefined> => idbGet(origin, CANISTER_STORE);

/** Records the senders resolved for `origin` from its well-known. */
export const cacheCanisters = (
  origin: string,
  canisterIds: string[],
): Promise<void> =>
  idbSet(origin, { canisterIds, resolvedAtMillis: Date.now() }, CANISTER_STORE);

/** The service worker's read side: the stored credential for `origin`, if any. */
export const loadNotificationCredential = (
  origin: string,
): Promise<NotificationCredentialRecord | undefined> =>
  idbGet(origin, CREDENTIAL_STORE);

/** Rebuilds the delegation identity the credential stands for. */
export const notificationIdentity = async (
  record: NotificationCredentialRecord,
): Promise<DelegationIdentity> => {
  const identity = await ECDSAKeyIdentity.fromKeyPair(record.keyPair);
  const chain = DelegationChain.fromJSON(JSON.parse(record.chainJson));
  return DelegationIdentity.fromDelegation(identity, chain);
};
