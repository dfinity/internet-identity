// Subscribes this browser to push and registers it with the canister: a fresh
// VAPID key, a signed JWT pool, then set_webpush_subscription. The signing key
// is kept (see vapidKeyStore) so the pool can be refreshed later without a new
// subscription. Shared by opt-in, the settings toggle, and the reconcile pass.

import { throwCanisterError } from "$lib/utils/utils";
import { generateVapidKeypair, signJwtPool } from "./vapidPool";
import { relayOriginOf, subscribeToPush } from "./pushSubscription";
import { storeVapidKey } from "./vapidKeyStore";
import { browserKeyActor } from "./browserActor";

/**
 * Subscribes and registers the device, returning the relay endpoint. Assumes
 * notification permission is already granted. `subscribeToPush` drops any stale
 * subscription first, so this is safe to call to replace a rotated one.
 */
export const subscribeAndRegisterDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const endpoint = await subscribeToPush(publicKeyRaw);
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    privateKey,
    relayOriginOf(endpoint),
    issuedAtNs,
  );
  const actor = await browserKeyActor(identityNumber);
  await actor
    .set_webpush_subscription({
      anchor_number: identityNumber,
      endpoint,
      vapid_public_key: publicKeyRaw,
      jwt_signatures: signatures,
      jwt_issued_at_ns: issuedAtNs,
    })
    .then(throwCanisterError);
  await storeVapidKey({ endpoint, privateKey, publicKeyRaw });
  return endpoint;
};
