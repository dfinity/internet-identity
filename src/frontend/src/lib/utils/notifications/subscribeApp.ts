/**
 * Subscribes the Home Screen app to push and registers that with the canister.
 *
 * The browser's own equivalent in `subscribeDevice.ts` registers per identity, because a
 * browser key is per identity and the row it writes is the anchor's. The app's is not:
 * one installed app has one service worker registration and one subscription, and every
 * identity pointing at it is delivered through that, so this call names no anchor and is
 * made once rather than once per identity.
 */
import { throwCanisterError } from "$lib/utils/utils";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { generateVapidKeypair, signJwtPool } from "./vapidPool";
import {
  currentDeviceSubscription,
  relayOriginOf,
  subscribeToPush,
} from "./pushSubscription";
import {
  loadVapidKey,
  storeVapidKey,
  type StoredVapidKey,
} from "./vapidKeyStore";

const registerStoredApp = async (
  actor: ActorSubclass<_SERVICE>,
  { endpoint, privateKey, publicKeyRaw }: StoredVapidKey,
): Promise<string> => {
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    privateKey,
    relayOriginOf(endpoint),
    issuedAtNs,
  );
  await actor
    .set_notification_app_subscription({
      endpoint,
      vapid_public_key: publicKeyRaw,
      jwt_signatures: signatures,
      jwt_issued_at_ns: issuedAtNs,
    })
    .then(throwCanisterError);
  return endpoint;
};

/**
 * Registers the app for push, reusing the subscription its partition already holds.
 *
 * Run on every launch and not only the one that linked, because what is registered can
 * go: the push service hands out a new endpoint whenever it feels like it, and opening
 * the app is the only moment anything here can write the new one.
 */
export const ensureRegisteredApp = async (
  actor: ActorSubclass<_SERVICE>,
): Promise<string> => {
  const stored = await loadVapidKey();
  const subscription = await currentDeviceSubscription();
  if (
    stored !== undefined &&
    subscription !== undefined &&
    stored.endpoint === subscription.endpoint
  ) {
    return registerStoredApp(actor, stored);
  }
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const endpoint = await subscribeToPush(publicKeyRaw);
  const fresh = { endpoint, privateKey, publicKeyRaw };
  await registerStoredApp(actor, fresh);
  await storeVapidKey(fresh);
  return endpoint;
};
