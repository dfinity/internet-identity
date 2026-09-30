/**
 * The delegation a service worker pulls a notification's content with.
 *
 * Internet Identity signs it for one app and one account, so a worker holds one per
 * pair and mints a new one when it has none or the one it has has expired. The session
 * key is generated here and never leaves this origin; the delegation is worth no more
 * than the content it can read.
 */

import { createStore, get as idbGet, set as idbSet } from "idb-keyval";
import type { ActorSubclass, Identity } from "@icp-sdk/core/agent";
import {
  AttributesIdentity,
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { transformSignedDelegation } from "$lib/utils/utils";
import { config } from "./workerConfig";

const PULL_STORE = createStore("ii-notification-pull", "delegations");

/** Minted again this long before it expires, rather than at the last moment. */
const RENEW_BEFORE_MILLIS = 60_000;

interface StoredPull {
  keyPair: CryptoKeyPair;
  /** `DelegationChain.toJSON()`, which is what survives structured cloning. */
  chain: unknown;
  /** Nanoseconds since the epoch, as the delegation carries it. */
  expiration: bigint;
  senderInfo: Uint8Array;
  senderInfoSignature: Uint8Array;
}

export interface PullTarget {
  origin: string;
  /** `undefined` is the unreserved default account. */
  accountNumber?: bigint;
}

const storageKey = (identityNumber: bigint, target: PullTarget): string =>
  `${identityNumber}|${target.origin}|${target.accountNumber ?? ""}`;

const isUsable = (stored: StoredPull, nowMillis: number): boolean =>
  // The page targets ES2019, so no BigInt literals.
  Number(stored.expiration / BigInt(1_000_000)) - RENEW_BEFORE_MILLIS >
  nowMillis;

/** What a stored pull becomes: an identity whose calls carry II's `sender_info`. */
const identityOf = async (stored: StoredPull): Promise<Identity> =>
  new AttributesIdentity({
    inner: DelegationIdentity.fromDelegation(
      await ECDSAKeyIdentity.fromKeyPair(stored.keyPair),
      DelegationChain.fromJSON(JSON.stringify(stored.chain)),
    ),
    attributes: {
      data: stored.senderInfo,
      signature: stored.senderInfoSignature,
    },
    signer: { canisterId: Principal.fromText(config.canisterId) },
  });

/** The delegation held for this app and account, or `undefined` where there is none
 *  left to use. */
export const loadPullIdentity = async ({
  identityNumber,
  target,
  nowMillis,
}: {
  identityNumber: bigint;
  target: PullTarget;
  nowMillis: number;
}): Promise<Identity | undefined> => {
  const stored = await idbGet<StoredPull>(
    storageKey(identityNumber, target),
    PULL_STORE,
  ).catch(() => undefined);
  if (stored === undefined || !isUsable(stored, nowMillis)) {
    return undefined;
  }
  return identityOf(stored);
};

/**
 * Asks Internet Identity for a delegation for this app and account and keeps it.
 *
 * The prepare call is what earns the worker another wake-up, so a caller that cannot
 * show the notification yet may stop here and show it on the next one.
 */
export const mintPullIdentity = async ({
  actor,
  identityNumber,
  target,
}: {
  actor: ActorSubclass<_SERVICE>;
  identityNumber: bigint;
  target: PullTarget;
}): Promise<Identity | undefined> => {
  const session = await ECDSAKeyIdentity.generate({ extractable: false });
  const sessionKey = new Uint8Array(session.getPublicKey().toDer());
  const accountNumber =
    target.accountNumber === undefined ? [] : [target.accountNumber];

  const prepared = await actor.prepare_notification_delegation({
    anchor_number: identityNumber,
    origin: target.origin,
    account_number: accountNumber as [] | [bigint],
    session_key: sessionKey,
  });
  if ("Err" in prepared) {
    return undefined;
  }

  const fetched = await actor.get_notification_delegation({
    anchor_number: identityNumber,
    origin: target.origin,
    account_number: accountNumber as [] | [bigint],
    session_key: sessionKey,
    expiration: prepared.Ok.expiration,
  });
  if ("Err" in fetched) {
    return undefined;
  }

  const chain = DelegationChain.fromDelegations(
    [transformSignedDelegation(fetched.Ok.signed_delegation)],
    new Uint8Array(prepared.Ok.user_key),
  );
  const stored: StoredPull = {
    keyPair: session.getKeyPair(),
    chain: JSON.parse(JSON.stringify(chain.toJSON())),
    expiration: prepared.Ok.expiration,
    senderInfo: new Uint8Array(prepared.Ok.sender_info),
    senderInfoSignature: new Uint8Array(fetched.Ok.sender_info_signature),
  };
  await idbSet(storageKey(identityNumber, target), stored, PULL_STORE).catch(
    () => {
      // A worker that cannot keep the delegation mints one per wake-up instead.
    },
  );

  return identityOf(stored);
};
