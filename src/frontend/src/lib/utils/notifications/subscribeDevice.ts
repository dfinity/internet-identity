// Subscribes this browser to push and registers it with the canister: a fresh
// VAPID key, a signed JWT pool, then set_webpush_subscription. The signing key
// is kept (see vapidKeyStore) so the pool can be refreshed later without a new
// subscription. Shared by opt-in, the settings toggle, and the reconcile pass.

import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { millisToNanos } from "$lib/utils/time";
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
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  SetWebPushSubscriptionError,
} from "$lib/generated/internet_identity_types";

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
  /** Who to call as. The Home Screen app signs as itself, and what it registers lands on its own record; everyone else signs as the browser key this sign-in registered. */
  caller?: ActorSubclass<_SERVICE>,
): Promise<string> => {
  const issuedAtNs = millisToNanos(Date.now());
  const signatures = await signJwtPool(
    privateKey,
    relayOriginOf(endpoint),
    issuedAtNs,
  );
  const actor = caller ?? (await browserKeyActor(identityNumber));
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

/** `minted` says whether the subscription is fresh, because a fresh key belongs on disk only once the canister has taken the endpoint it was made for. */
const deviceSubscription = async (): Promise<{
  subscription: StoredVapidKey;
  minted: boolean;
}> => {
  const stored = await loadVapidKey();
  const live = await currentDeviceSubscription();
  if (
    stored !== undefined &&
    live !== undefined &&
    stored.endpoint === live.endpoint
  ) {
    return { subscription: stored, minted: false };
  }
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const endpoint = await subscribeToPush(publicKeyRaw);
  return { subscription: { endpoint, privateKey, publicKeyRaw }, minted: true };
};

/**
 * Registers this browser for `identityNumber`, reusing the subscription it already
 * holds. Only a browser without a usable one subscribes afresh, since subscribing
 * drops the endpoint every other identity on this browser is registered with.
 */
export const ensureRegisteredDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const { subscription, minted } = await deviceSubscription();
  await registerStoredDevice(identityNumber, subscription);
  if (minted) {
    await storeVapidKey(subscription);
  }
  return subscription.endpoint;
};

/** `InvalidBrowserKey` is the anchor named not reaching the caller, which says nothing about the next one.
 * `StaleJwtPool` is about the endpoint, which every anchor shares, so the anchors after this one would be refused for the same reason. */
const anotherAnchorMayAnswer = (error: unknown): boolean => {
  if (!isCanisterError<SetWebPushSubscriptionError>(error)) {
    return false;
  }
  switch (error.type) {
    case "InvalidBrowserKey":
      return true;
    case "StaleJwtPool":
    case "InternalCanisterError":
      return false;
  }
};

/** Taking the first anchor alone would let a single identity whose reference is switched off end every launch.
 * One subscription for the whole walk: minting per anchor would hand out a new endpoint for each one. */
export const registerForAnyAnchor = async (
  anchors: bigint[],
  caller: ActorSubclass<_SERVICE>,
): Promise<boolean> => {
  const { subscription, minted } = await deviceSubscription();
  for (const anchor of anchors) {
    try {
      await registerStoredDevice(anchor, subscription, caller);
    } catch (error) {
      if (anotherAnchorMayAnswer(error)) {
        continue;
      }
      throw error;
    }
    if (minted) {
      await storeVapidKey(subscription);
    }
    return true;
  }
  return false;
};
