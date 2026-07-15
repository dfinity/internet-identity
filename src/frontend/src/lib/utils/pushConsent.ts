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
import { fromBase64URL, throwTextCanisterError } from "$lib/utils/utils";

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
