// Both keys a browser needs before it can subscribe: the one every push is signed with,
// and a one-shot key it encrypts this subscription's `auth` secret to.
//
// An update call rather than a query. A query reply is signed by one replica, and a
// subscription binds to whatever application server key it is given, permanently, with
// nothing on this side to check that key against.
//
// Nothing is kept on this side. The transport half is minted for one subscribe and the
// canister drops it as soon as that subscribe spends it, so a held copy would be a key
// the next call could not use.

import { retryFor, throwCanisterError } from "$lib/utils/utils";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

export interface SubscriptionKeys {
  /** Internet Identity's own VAPID public key, which a browser subscribes with. */
  vapidPublicKey: Uint8Array;
  /** Minted for this caller and this attempt. */
  transportPublicKey: Uint8Array;
}

/** Enough asks to outlast the rounds the canister's own draw takes to come back. */
const ATTEMPTS = 4;

const askOnce = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<SubscriptionKeys> => {
  const response = await actor.prepare_webpush_subscription(identityNumber);
  if ("Pending" in response) {
    // Thrown so the ask is re-issued: the draw is in flight by now, and the wait before
    // the next one is what it needs to land.
    throw new Error("the randomness these keys come from is still being drawn");
  }
  const keys = await throwCanisterError(response);
  return {
    vapidPublicKey: new Uint8Array(keys.vapid_public_key),
    transportPublicKey: new Uint8Array(keys.transport_public_key),
  };
};

/**
 * Asks `actor` for the keys `identityNumber`'s next subscribe needs.
 *
 * The canister derives both from randomness it keeps pooled, and a pool nobody has
 * subscribed through for a while has none to serve. The first ask puts the draw in
 * flight and is told to come back; the reply to it lands a round or two later, which is
 * what the wait between asks is for.
 */
export const prepareSubscription = (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<SubscriptionKeys> =>
  retryFor(ATTEMPTS, () => askOnce(identityNumber, actor));
