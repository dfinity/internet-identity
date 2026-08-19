// The credential the service worker uses to pull a notification's content from
// the dApp. It is the user's ordinary per-app identity — a session key plus an
// II-signed account delegation scoped to the dApp's origin — persisted to
// IndexedDB (which a service worker can read, unlike localStorage). Interim
// mechanism: when revocable app sessions land, this is replaced wholesale by a
// revocable session under an AttributesIdentity wrapper.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { toPermissionsArg } from "$lib/utils/accessLevel";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { createStore, del as idbDel, set as idbSet } from "idb-keyval";

const CREDENTIAL_STORE = createStore("ii-notification-credentials", "keys");

export interface NotificationCredentialRecord {
  /** dApp origin, keyed on and also the routing target the SW pulls for. */
  origin: string;
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
  actor,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
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
    keyPair: sessionIdentity.getKeyPair(),
    chainJson: JSON.stringify(chain.toJSON()),
    expiresAtMillis: Number(expiration / BigInt(1_000_000)),
  };
};

export const storeNotificationCredential = (
  record: NotificationCredentialRecord,
): Promise<void> => idbSet(record.origin, record, CREDENTIAL_STORE);

export const purgeNotificationCredential = (origin: string): Promise<void> =>
  idbDel(origin, CREDENTIAL_STORE);
