// Subscribes this browser to push and registers it with the canister: a fresh
// VAPID key, a signed JWT pool, then webpush_subscribe_device. The signing key
// is kept (see vapidKeyStore) so the pool can be refreshed later without a new
// subscription. Shared by opt-in, the settings toggle, and the reconcile pass.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";
import { generateVapidKeypair, signJwtPool } from "./vapidPool";
import { relayOriginOf, subscribeToPush } from "./pushSubscription";
import { storeVapidKey } from "./vapidKeyStore";

/**
 * Subscribes and registers the device, returning the relay endpoint. Assumes
 * notification permission is already granted. `subscribeToPush` drops any stale
 * subscription first, so this is safe to call to replace a rotated one.
 */
export const subscribeAndRegisterDevice = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<string> => {
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const { endpoint, p256dh, auth } = await subscribeToPush(publicKeyRaw);
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    privateKey,
    relayOriginOf(endpoint),
    issuedAtNs,
  );
  await actor
    .webpush_subscribe_device(
      identityNumber,
      endpoint,
      p256dh,
      auth,
      publicKeyRaw,
      signatures,
      issuedAtNs,
    )
    .then(throwTextCanisterError);
  await storeVapidKey({ endpoint, privateKey, publicKeyRaw });
  return endpoint;
};
