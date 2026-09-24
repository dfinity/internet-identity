// Subscribes this browser to push and registers it with the canister: a fresh
// VAPID key, a signed JWT pool, then set_webpush_subscription. The signing key
// is kept (see vapidKeyStore) so the pool can be refreshed later without a new
// subscription. Shared by opt-in, the settings toggle, and the reconcile pass.

import { throwCanisterError } from "$lib/utils/utils";
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
import { browserKeyActor } from "./browserActor";

/**
 * Registers a subscription this browser already holds with `identityNumber`, signing a
 * fresh pool with the key it was minted under.
 *
 * The subscription and its VAPID key belong to the browser, not to one identity, so a
 * second identity signing in here registers what is already there. Rotating instead
 * would unsubscribe the endpoint every other identity's row still names.
 */
export const registerStoredDevice = async (
  identityNumber: bigint,
  { endpoint, privateKey, publicKeyRaw }: StoredVapidKey,
): Promise<string> => {
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
  return endpoint;
};

/**
 * Mints a subscription and a VAPID key for this browser, then registers it. Assumes
 * notification permission is already granted. `subscribeToPush` drops any prior
 * subscription, so this replaces one rather than adding to it, and every other
 * identity's stored row is left naming an endpoint that is gone: only call it where
 * the browser has no usable subscription of its own.
 */
export const subscribeAndRegisterDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const endpoint = await subscribeToPush(publicKeyRaw);
  const stored = { endpoint, privateKey, publicKeyRaw };
  await registerStoredDevice(identityNumber, stored);
  await storeVapidKey(stored);
  return endpoint;
};

/**
 * Registers this browser for `identityNumber`, reusing the subscription it already
 * holds. Only a browser without a usable one subscribes afresh, since subscribing
 * drops the endpoint every other identity on this browser is registered with.
 */
export const ensureRegisteredDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const stored = await loadVapidKey();
  const subscription = await currentDeviceSubscription();
  return stored !== undefined &&
    subscription !== undefined &&
    stored.endpoint === subscription.endpoint
    ? registerStoredDevice(identityNumber, stored)
    : subscribeAndRegisterDevice(identityNumber);
};
