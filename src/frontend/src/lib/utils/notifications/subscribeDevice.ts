// Subscribes this browser to push and registers it with the canister: subscribe with
// Internet Identity's own VAPID key, then set_webpush_subscription. Nothing is kept on
// this side, because the key that authorizes a push is the canister's. Shared by opt-in,
// the settings toggle, and the reconcile pass.

import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import {
  currentDeviceSubscription,
  subscribedWith,
  subscribeToPush,
} from "./pushSubscription";
import { vapidPublicKey } from "./vapidPublicKey";
import { derivePayloadKeys } from "./payloadKeys";
import { browserKeyActor } from "./browserActor";
import { registeredIdentityNumbers } from "$lib/stores/browser-key.store";
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
 * The subscription this browser can be woken through: the one it already holds where
 * that was minted with our key, and a fresh one otherwise.
 *
 * A subscription minted with any other application server key is one no push we sign
 * can reach, so reusing it would register an endpoint that is dead on arrival.
 *
 * `minted` says a new one replaced whatever was there, which is what obliges the caller
 * to re-register every other identity this browser holds a key for.
 */
const usableSubscription = async (): Promise<{
  subscription: PushSubscription;
  minted: boolean;
}> => {
  const applicationServerKey = await vapidPublicKey();
  const held = await currentDeviceSubscription();
  if (held !== undefined && subscribedWith(held, applicationServerKey)) {
    return { subscription: held, minted: false };
  }
  return {
    subscription: await subscribeToPush(applicationServerKey),
    minted: true,
  };
};

/**
 * Moves every other identity that was registered on this browser onto `subscription`.
 *
 * Subscribing drops the endpoint they were registered with, so without this each one is
 * left naming an address nothing is delivered to, and only its own owner going to
 * settings would ever find out.
 *
 * Only an identity the canister already holds a registration for is moved: one that
 * never turned notifications on, or turned them off again, has no registration to
 * repair, and writing one here would turn them on in its owner's name. Theirs is a
 * repair rather than what the caller asked for, so a refusal is left where it is.
 */
export const registerSiblings = async (
  registered: bigint,
  subscription: PushSubscription,
): Promise<void> => {
  const identityNumbers = await registeredIdentityNumbers().catch(() => []);
  await Promise.all(
    identityNumbers
      .filter((identityNumber) => identityNumber !== registered)
      .map(async (identityNumber) => {
        try {
          const actor = await browserKeyActor(identityNumber);
          const [held] = await actor.get_webpush_subscription_status({
            anchor_number: identityNumber,
          });
          if (held === undefined) {
            return;
          }
          await registerStoredDevice(identityNumber, subscription, actor);
        } catch {
          // Left where it is: this identity's owner is not the one waiting on an answer.
        }
      }),
  );
};

/**
 * Registers this browser for `identityNumber`, reusing the subscription it already
 * holds. Only a browser without a usable one subscribes afresh, since subscribing
 * drops the endpoint every other identity on this browser is registered with.
 */
export const ensureRegisteredDevice = async (
  identityNumber: bigint,
): Promise<string> => {
  const { subscription, minted } = await usableSubscription();
  await registerStoredDevice(identityNumber, subscription);
  if (minted) {
    await registerSiblings(identityNumber, subscription);
  }
  return subscription.endpoint;
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
): Promise<boolean> => {
  const { subscription: live } = await usableSubscription();

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
