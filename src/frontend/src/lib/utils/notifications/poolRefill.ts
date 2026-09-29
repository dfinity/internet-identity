/**
 * Keeping the VAPID JWT pool stocked.
 *
 * The pool is spent by elapsed time: each signature covers one 24-hour window, so a
 * pool of 30 runs dry a month after it was signed and the canister can no longer wake
 * this browser. The page tops it up whenever it looks at the notification settings,
 * which is no use to a browser whose user never opens Internet Identity again — so a
 * service worker handling a wake-up tops it up too.
 *
 * The signing key is the browser's own, non-extractable, and kept in IndexedDB, so a
 * worker can sign with it exactly as the page does.
 */

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { loadVapidKey } from "./vapidKeyStore";
import {
  JWT_POOL_REFRESH_THRESHOLD_WINDOWS,
  relayOriginOf,
  signJwtPool,
  windowsRemaining,
} from "./vapidPool";

/**
 * Signs and registers a fresh pool where the stored one is running out. Answers
 * whether it did, which is what a test has to go on.
 *
 * Does nothing where this browser holds no key, where the canister holds a
 * registration for a different endpoint — another identity's re-subscribe left that
 * behind, and re-registering it here would name an endpoint that is gone — or where
 * the pool still covers enough windows.
 */
export const refillJwtPool = async ({
  actor,
  identityNumber,
  nowNs,
}: {
  actor: ActorSubclass<_SERVICE>;
  identityNumber: bigint;
  nowNs: bigint;
}): Promise<boolean> => {
  const stored = await loadVapidKey();
  if (stored === undefined) {
    return false;
  }
  const [status] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
  });
  if (status === undefined || status.endpoint !== stored.endpoint) {
    return false;
  }
  const remaining = windowsRemaining({
    poolLen: status.pool_len,
    issuedAtNs: status.issued_at_ns,
    nowNs,
  });
  if (remaining >= JWT_POOL_REFRESH_THRESHOLD_WINDOWS) {
    return false;
  }

  const signatures = await signJwtPool(
    stored.privateKey,
    relayOriginOf(stored.endpoint),
    nowNs,
  );
  await actor
    .set_webpush_subscription({
      anchor_number: identityNumber,
      endpoint: stored.endpoint,
      vapid_public_key: stored.publicKeyRaw,
      jwt_signatures: signatures,
      jwt_issued_at_ns: nowNs,
    })
    .then(throwCanisterError);
  return true;
};
