// Subscribes this browser to push and registers it with the canister: ask for the keys,
// subscribe with Internet Identity's own VAPID key, encrypt this subscription's auth
// secret to the one-shot transport key that came with it, then set_webpush_subscription.
// Nothing is kept on this side, because the key that authorizes a push is the canister's.
// Shared by opt-in, the settings toggle, and the reconcile pass.

import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import {
  currentDeviceSubscription,
  subscribedWith,
  subscribeToPush,
} from "./pushSubscription";
import {
  prepareSubscription,
  type SubscriptionKeys,
} from "./prepareSubscription";
import { sealSubscriptionAuth } from "./transportSeal";
import { browserKeyActor } from "./browserActor";
import { registeredIdentityNumbers } from "$lib/stores/browser-key.store";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  SetWebPushSubscriptionError,
} from "$lib/generated/internet_identity_types";

/** Whether the canister already has `identityNumber` registered at this address. */
const registeredAt = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  subscription: PushSubscription,
): Promise<boolean> => {
  const [held] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
  });
  return held?.endpoint === subscription.endpoint;
};

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
  /** Keys already fetched for this identity, where the caller needed the VAPID half first. */
  fetched?: SubscriptionKeys,
): Promise<string> => {
  const actor = caller ?? (await browserKeyActor(identityNumber));
  // Nothing to do where the canister already holds this address. Re-registering would
  // mint a transport key and run the whole derivation to arrive at the record it already
  // has, and the Home Screen app comes through here on every launch.
  if (
    fetched === undefined &&
    (await registeredAt(actor, identityNumber, subscription))
  ) {
    return subscription.endpoint;
  }
  // Its own fetch where none was handed down: the transport key is minted per caller and
  // spent by one subscribe, so no two registrations can share one.
  const { transportPublicKey } =
    fetched ?? (await prepareSubscription(identityNumber, actor));
  const { p256dh, transportKey, sealedAuth } = await sealSubscriptionAuth(
    subscription,
    transportPublicKey,
  );
  await actor
    .set_webpush_subscription({
      anchor_number: identityNumber,
      endpoint: subscription.endpoint,
      p256dh,
      transport_key: transportKey,
      sealed_auth: sealedAuth,
    })
    .then(throwCanisterError);
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
const usableSubscription = async (
  keys: SubscriptionKeys,
): Promise<{
  subscription: PushSubscription;
  minted: boolean;
}> => {
  const held = await currentDeviceSubscription();
  if (held !== undefined && subscribedWith(held, keys.vapidPublicKey)) {
    return { subscription: held, minted: false };
  }
  return {
    subscription: await subscribeToPush(keys.vapidPublicKey),
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
 * repair, and writing one here would turn them on in its owner's name. A browser signed
 * out between that read and this write is refused by the canister, which is where that
 * rule belongs. Theirs is a repair rather than what the caller asked for, so a refusal
 * is left where it is.
 */
const registerSiblings = async (
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
  const actor = await browserKeyActor(identityNumber);
  const keys = await prepareSubscription(identityNumber, actor);
  const { subscription, minted } = await usableSubscription(keys);
  // They are siblings: every identity here registers the same subscription under its own
  // browser key, and the one that asked is distinguished only in that its answer is the
  // one being waited on. Started together, so a failure of that one cannot leave the
  // others on an endpoint this browser has already dropped.
  const [asked] = await Promise.allSettled([
    registerStoredDevice(identityNumber, subscription, actor, keys),
    ...(minted ? [registerSiblings(identityNumber, subscription)] : []),
  ]);
  if (asked.status === "rejected") {
    throw asked.reason;
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
    // The transport key is this browser's, not this anchor's, so the next anchor would
    // find it gone too: the remedy is a fresh fetch, which the next call does anyway.
    case "TransportKeyExpired":
      return false;
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
  const [first] = anchors;
  if (first === undefined) {
    return false;
  }
  // Before anything is fetched, because every launch of the installed app comes through
  // here and the push service usually hands back the address it already gave us. An
  // anchor already registered at it needs no key, no derivation and no write.
  const held = await currentDeviceSubscription();
  if (held !== undefined) {
    for (const anchor of anchors) {
      if (await registeredAt(caller, anchor, held)) {
        return true;
      }
    }
  }

  const { subscription: live } = await usableSubscription(
    await prepareSubscription(first, caller),
  );

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
