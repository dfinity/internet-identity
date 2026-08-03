/**
 * Push-notification consent: the set of dApp origins the identity has
 * granted permission to send push notifications to, via `push_grant_consent`
 * during `/authorize`. Read via `push_list_consented_origins` and revoked via
 * `push_revoke_consent`, both authenticated as the identity so only the user
 * (never the dApp) can change what's granted. The Settings UI is the only
 * place this list is surfaced.
 *
 * Also wraps the device-level subscription surface (Option A: II hosts the
 * Service Worker and receives Web Push traffic for every consented dApp).
 * `push_subscribe_device` / `push_unsubscribe_device` are keyed by
 * `(anchor, sha256(endpoint))`, so each device subscribes independently.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  bufFromBufLike,
  fromBase64URL,
  throwTextCanisterError,
} from "$lib/utils/utils";

/** List the origins this identity has granted push-notification consent to. */
export const listConsentedOrigins = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<string[]> => actor.push_list_consented_origins(identityNumber);

/** Revoke a previously-granted consent for `origin`. */
export const revokeConsent = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  origin: string,
): Promise<void> => {
  await actor
    .push_revoke_consent(identityNumber, origin)
    .then(throwTextCanisterError);
};

/**
 * The VAPID public key (65-byte uncompressed SEC1 P-256 point) to pass as
 * `applicationServerKey` to `pushManager.subscribe()`.
 */
export const getVapidPublicKey = async (
  actor: ActorSubclass<_SERVICE>,
): Promise<Uint8Array> => {
  const key = await actor.push_vapid_public_key();
  return key instanceof Uint8Array ? key : new Uint8Array(key);
};

/**
 * Register a Web Push subscription for `anchor_number` on this device.
 * `p256dh` and `auth` are the base64url-encoded keys off
 * `PushSubscription.toJSON().keys` — decoded here so callers can pass the
 * subscription's own string fields straight through.
 */
export const subscribeDevice = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  endpoint: string,
  p256dh: string,
  auth: string,
): Promise<void> => {
  await actor
    .push_subscribe_device(
      identityNumber,
      endpoint,
      fromBase64URL(p256dh),
      fromBase64URL(auth),
    )
    .then(throwTextCanisterError);
};

/**
 * Register this device for push and record it against `identityNumber`,
 * returning the live subscription.
 *
 * The awkward part is that a browser allows only **one** push subscription per
 * service worker, permanently bound to the `applicationServerKey` it was created
 * with. `subscribe()` rejects outright when asked for a different key:
 *
 *   "A subscription with a different applicationServerKey ... already exists;
 *    to change the applicationServerKey, unsubscribe then resubscribe."
 *
 * That happens whenever II's VAPID key changes — which it did when the key moved
 * from a hardcoded pair to one generated on the canister — leaving every browser
 * that had subscribed unable to re-enable. So an existing subscription is reused
 * when its key still matches, and replaced when it doesn't.
 *
 * Replacing rotates the endpoint, which strands rows other identities on this
 * browser hold for the old one. That is acceptable precisely here: a subscription
 * bound to a key II no longer holds is already undeliverable for *every*
 * identity, so rotating fixes them rather than harming them. This anchor's own
 * stale row is cleaned up explicitly; the rest need `pushsubscriptionchange`
 * handling, which is tracked separately.
 */
export const ensureDeviceSubscription = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<PushSubscription> => {
  const registration =
    await navigator.serviceWorker.register("/service-worker.js");
  await navigator.serviceWorker.ready;
  const vapidPublicKey = await getVapidPublicKey(actor);

  const existing = await registration.pushManager.getSubscription();
  if (existing !== null) {
    const existingKey = existing.options.applicationServerKey;
    if (
      existingKey !== null &&
      existingKey !== undefined &&
      sameBytes(new Uint8Array(existingKey), vapidPublicKey)
    ) {
      // Same key: the subscription is still valid, so re-registering it is
      // enough. Idempotent server-side, keyed by the endpoint's hash.
      await registerSubscription(actor, identityNumber, existing);
      return existing;
    }
    // Different key (or a subscription with no key at all): unusable. Drop this
    // anchor's row for the dead endpoint first, best-effort — it may never have
    // been recorded — so it can't linger as an undeliverable target.
    await unsubscribeDevice(actor, identityNumber, existing.endpoint).catch(
      () => undefined,
    );
    await existing.unsubscribe();
  }

  const subscription = await registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: bufFromBufLike(vapidPublicKey),
  });
  await registerSubscription(actor, identityNumber, subscription);
  return subscription;
};

const sameBytes = (a: Uint8Array, b: Uint8Array): boolean =>
  a.length === b.length && a.every((byte, index) => byte === b[index]);

const registerSubscription = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  subscription: PushSubscription,
): Promise<void> => {
  const { endpoint, keys } = subscription.toJSON() as {
    endpoint: string;
    keys: { p256dh: string; auth: string };
  };
  await subscribeDevice(
    actor,
    identityNumber,
    endpoint,
    keys.p256dh,
    keys.auth,
  );
};

/** Remove this device's subscription (identified by its push `endpoint`). */
export const unsubscribeDevice = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  endpoint: string,
): Promise<void> => {
  await actor
    .push_unsubscribe_device(identityNumber, endpoint)
    .then(throwTextCanisterError);
};
