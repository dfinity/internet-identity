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

import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  GetNotificationKeysError,
} from "$lib/generated/internet_identity_types";

export interface NotificationKeys {
  /** Internet Identity's own VAPID public key, which a browser subscribes with. */
  vapidPublicKey: Uint8Array;
  /** Minted for this caller and this attempt. */
  transportPublicKey: Uint8Array;
}

const askOnce = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<NotificationKeys> => {
  const keys = await actor
    .get_notification_keys(identityNumber)
    .then(throwCanisterError);
  return {
    vapidPublicKey: new Uint8Array(keys.vapid_public_key),
    transportPublicKey: new Uint8Array(keys.transport_public_key),
  };
};

/**
 * Asks `actor` for the keys `identityNumber`'s next subscribe needs.
 *
 * The canister derives both from randomness it keeps pooled, and a pool nobody has
 * subscribed through for a while has none to serve. It answers `TryAgain` and draws,
 * so the second ask is the one that finds it drawn.
 */
export const notificationKeys = (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<NotificationKeys> =>
  askOnce(identityNumber, actor).catch((error: unknown) => {
    if (
      !isCanisterError<GetNotificationKeysError>(error) ||
      error.type !== "TryAgain"
    ) {
      throw error;
    }
    return askOnce(identityNumber, actor);
  });
