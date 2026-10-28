// Subscribes this browser to push and registers it with the canister: subscribe with
// Internet Identity's own VAPID key, then set_webpush_subscription. Nothing is kept on
// this side, because the key that authorizes a push is the canister's. Shared by opt-in,
// the settings toggle, and the reconcile pass.

import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { currentDeviceSubscription, subscribeToPush } from "./pushSubscription";
import { vapidPublicKey } from "./vapidPublicKey";
import { derivePayloadKeys } from "./payloadKeys";
import { browserKeyActor } from "./browserActor";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  SetWebPushSubscriptionError,
} from "$lib/generated/internet_identity_types";

/**
 * Registers a subscription this browser already holds with `identityNumber`.
 *
 * The subscription belongs to the browser, not to one identity, so a second identity
 * signing in here registers what is already there. Rotating instead would unsubscribe
 * the endpoint every other identity's row still names.
 */
export const registerStoredDevice = async (
  identityNumber: bigint,
  /** What the payload keys are derived from: the registration this call is about. */
  subscription: PushSubscription,
  /** Who to call as. The Home Screen app signs as itself, and what it registers lands on its own record; everyone else signs as the browser key this sign-in registered. */
  caller?: ActorSubclass<_SERVICE>,
): Promise<string> => {
  const actor = caller ?? (await browserKeyActor(identityNumber));
  const { payloadIkm, payloadKeyid } = await derivePayloadKeys(subscription);
  await actor
    .set_webpush_subscription({
      anchor_number: identityNumber,
      endpoint: subscription.endpoint,
      payload_ikm: payloadIkm,
      payload_keyid: payloadKeyid,
    })
    .then(throwCanisterError);
  return subscription.endpoint;
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
  const subscription = await subscribeToPush(await vapidPublicKey());
  await registerStoredDevice(identityNumber, subscription);
  return subscription.endpoint;
};

/**
 * Registers this browser for `identityNumber`, reusing the subscription it already
 * holds. Only a browser without a usable one subscribes afresh, since subscribing
 * drops the endpoint every other identity on this browser is registered with.
 */
export const ensureRegisteredDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const live =
    (await currentDeviceSubscription()) ??
    (await subscribeToPush(await vapidPublicKey()));
  await registerStoredDevice(identityNumber, live);
  return live.endpoint;
};

/** `InvalidBrowserKey` is the anchor named not reaching the caller, which says nothing
 * about the next one. */
const anotherAnchorMayAnswer = (error: unknown): boolean => {
  if (!isCanisterError<SetWebPushSubscriptionError>(error)) {
    return false;
  }
  switch (error.type) {
    case "InvalidBrowserKey":
      return true;
    case "InternalCanisterError":
      return false;
  }
};

/** Taking the first anchor alone would let a single identity whose reference is switched off end every launch.
 * One subscription for the whole walk: minting per anchor would hand out a new endpoint for each one. */
export const registerForAnyAnchor = async (
  anchors: bigint[],
  caller: ActorSubclass<_SERVICE>,
  /** The app record the subscription hangs on, where the caller knows it. */
  appId?: bigint,
): Promise<boolean> => {
  const live =
    (await currentDeviceSubscription()) ??
    (await subscribeToPush(await vapidPublicKey()));

  for (const anchor of anchors) {
    try {
      await registerStoredDevice(anchor, live, caller);
    } catch (error) {
      if (anotherAnchorMayAnswer(error)) {
        continue;
      }
      throw error;
    }
    return true;
  }
  return false;
};
