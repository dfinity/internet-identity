// Keeps this browser's push registration healthy. The device toggle owns the
// subscription; per-app consent is a separate list and is never touched here.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { signJwtPool, windowsRemaining } from "./vapidPool";
import {
  currentDeviceSubscription,
  isPushSupported,
  relayOriginOf,
} from "./pushSubscription";
import {
  registerStoredDevice,
  subscribeAndRegisterDevice,
} from "./subscribeDevice";
import { loadVapidKey } from "./vapidKeyStore";
import { currentBrowserId } from "$lib/stores/browser-key.store";
import { browserKeyActor } from "./browserActor";

export { currentDeviceSubscription };

// Windows left in the pool below which the reconcile tops it up. The pool covers 30
// days, so this leaves a week of headroom.
const JWT_POOL_REFRESH_THRESHOLD = 10;

/**
 * Run on authenticated boot. Re-subscribes when the browser rotated or dropped
 * its subscription, so the canister stops knowing a dead endpoint, and tops up
 * the JWT pool before it runs out. No-op for a browser that never turned
 * notifications on.
 */
export const reconcileDeviceNotifications = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<void> => {
  if (!isPushSupported() || Notification.permission !== "granted") {
    return;
  }
  const subscription = await currentDeviceSubscription();
  const stored = await loadVapidKey();
  if (subscription === undefined && stored === undefined) {
    return;
  }

  // The canister knows this browser only if the live subscription matches the key we
  // still hold. Anything else means subscribe and register afresh.
  if (
    subscription === undefined ||
    stored === undefined ||
    stored.endpoint !== subscription.endpoint
  ) {
    // Re-subscribing overwrites the same row, so a rotated endpoint needs no removal.
    await subscribeAndRegisterDevice(identityNumber);
    return;
  }

  // The browser's subscription is sound, so what is left is this identity's own row.
  const browserId = await currentBrowserId(identityNumber);
  if (browserId === undefined) {
    return;
  }
  const [status] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
    browser_id: browserId,
  });
  // Nothing registered is what a second identity on this browser looks like, and a
  // different endpoint is what another identity's re-subscribe left behind. Either
  // way, register what the browser already holds rather than rotating it away.
  if (status === undefined || status.endpoint !== stored.endpoint) {
    await registerStoredDevice(identityNumber, stored);
    return;
  }
  const remaining = windowsRemaining({
    poolLen: status.pool_len,
    issuedAtNs: status.issued_at_ns,
    nowNs: BigInt(Date.now()) * BigInt(1_000_000),
  });
  if (remaining >= JWT_POOL_REFRESH_THRESHOLD) {
    return;
  }
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    stored.privateKey,
    relayOriginOf(stored.endpoint),
    issuedAtNs,
  );
  // The call that registers is also the call that tops up: the same endpoint and key
  // with a pool that moves forward.
  const browserActor = await browserKeyActor(identityNumber);
  await browserActor
    .set_webpush_subscription({
      anchor_number: identityNumber,
      endpoint: stored.endpoint,
      vapid_public_key: stored.publicKeyRaw,
      jwt_signatures: signatures,
      jwt_issued_at_ns: issuedAtNs,
    })
    .then(throwCanisterError);
};
