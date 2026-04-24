/**
 * Web Push notification helpers.
 *
 * Browser flow on first opt-in (must run inside a user gesture, otherwise
 * `Notification.requestPermission()` is rejected by modern browsers):
 *
 *   1. Register the service worker at `/sw.js`.
 *   2. Ask for notification permission.
 *   3. Fetch the canister's VAPID public key (triggers canister-side
 *      generation if this is the first subscriber).
 *   4. Call `PushManager.subscribe()` with that key as `applicationServerKey`.
 *   5. Forward the resulting `PushSubscription` to the canister so it can POST
 *      to the endpoint when new emails arrive.
 *
 * V1 does not use the `p256dh`/`auth` keys for payload encryption — the
 * canister sends empty-body pushes. We still send them to the canister so
 * they're stored for when we add RFC 8291 encryption later.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

export const isPushSupported = (): boolean =>
  typeof window !== "undefined" &&
  "serviceWorker" in navigator &&
  "PushManager" in window &&
  "Notification" in window;

/** Returns the active subscription if one exists for this browser. */
export const getExistingSubscription =
  async (): Promise<PushSubscription | null> => {
    if (!isPushSupported()) return null;
    const registration = await navigator.serviceWorker.getRegistration("/");
    if (!registration) return null;
    return registration.pushManager.getSubscription();
  };

/** Convenience wrapper for "is this browser currently subscribed?". */
export const isCurrentlySubscribed = async (): Promise<boolean> => {
  const sub = await getExistingSubscription();
  return sub !== null;
};

/**
 * PoC side-channel: posts the newest email's sender/subject to the active
 * service worker so the next `push` event can show meaningful content
 * instead of the generic fallback. Silently no-ops when no service worker
 * is registered or active.
 *
 * Not a long-term solution — the service worker's in-memory cache is wiped
 * whenever the worker is terminated (which can happen any time the page is
 * closed or idle). The proper fix is RFC 8291 payload encryption in the
 * canister so the push itself carries the metadata. Until that ships this
 * gives the postbox page a way to keep the SW's cache warm.
 */
export const postLatestEmailToServiceWorker = async (email: {
  sender: string;
  subject: string;
}): Promise<void> => {
  if (!("serviceWorker" in navigator)) return;
  const registration = await navigator.serviceWorker.getRegistration("/");
  if (!registration?.active) return;
  registration.active.postMessage({
    type: "LATEST_EMAIL",
    from: email.sender,
    subject: email.subject,
  });
};

/**
 * Full opt-in flow. Must be called from within a user gesture (e.g. a click
 * handler) so the browser allows `Notification.requestPermission()`.
 *
 * Returns the created subscription on success, or `null` if the user denied
 * the permission prompt. Throws for other errors (network, canister).
 */
export const subscribeToPush = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<PushSubscription | null> => {
  if (!isPushSupported()) {
    throw new Error("Push notifications are not supported in this browser");
  }

  // 1. Register the service worker (idempotent).
  const registration = await navigator.serviceWorker.register("/sw.js", {
    scope: "/",
  });
  // Wait for it to be active — `pushManager.subscribe` can fail otherwise.
  if (registration.installing || registration.waiting) {
    await navigator.serviceWorker.ready;
  }

  // 2. Ask for notification permission.
  const permission = await Notification.requestPermission();
  if (permission !== "granted") {
    return null;
  }

  // 3. Fetch (or generate) the VAPID public key from the canister.
  //    `push_init_vapid_key` is an update call that generates the key if
  //    it doesn't exist yet, so the first subscriber on this canister pays
  //    an extra raw_rand + state write. Subsequent subscribers get the
  //    already-stored key immediately.
  const initResult = await actor.push_init_vapid_key();
  if ("Err" in initResult) {
    throw new Error(
      `Failed to initialize VAPID key: ${JSON.stringify(initResult.Err)}`,
    );
  }
  // Copy into a fresh ArrayBuffer-backed Uint8Array. PushManager.subscribe
  // rejects Uint8Array<SharedArrayBuffer>, which the generated TS types
  // include as a possibility.
  const vapidSrc =
    initResult.Ok instanceof Uint8Array
      ? initResult.Ok
      : new Uint8Array(initResult.Ok);
  const vapidKey = new Uint8Array(new ArrayBuffer(vapidSrc.length));
  vapidKey.set(vapidSrc);

  // 4. Subscribe with PushManager.
  const subscription = await registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: vapidKey,
  });

  // 5. Send the subscription to the canister.
  const json = subscription.toJSON();
  if (
    json.endpoint === undefined ||
    json.keys?.p256dh === undefined ||
    json.keys?.auth === undefined
  ) {
    // Revert the local subscription so the user isn't in a weird state.
    await subscription.unsubscribe();
    throw new Error("Browser returned an incomplete PushSubscription");
  }

  const result = await actor.push_subscribe(identityNumber, {
    endpoint: json.endpoint,
    p256dh: base64UrlToBytes(json.keys.p256dh),
    auth: base64UrlToBytes(json.keys.auth),
  });
  if ("Err" in result) {
    await subscription.unsubscribe();
    throw new Error(
      `Canister rejected subscription: ${JSON.stringify(result.Err)}`,
    );
  }

  return subscription;
};

/**
 * Removes the current browser's subscription from the canister and from
 * the browser's PushManager. Silently no-ops if not subscribed.
 */
export const unsubscribeFromPush = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<void> => {
  const subscription = await getExistingSubscription();
  if (subscription === null) return;

  // Tell the canister first — if that fails, we still want to unsubscribe
  // locally so the user sees the UI flip off.
  try {
    await actor.push_unsubscribe(identityNumber, subscription.endpoint);
  } catch {
    // ignore
  }
  await subscription.unsubscribe();
};

/**
 * Fetches the canister's VAPID public key. Returns `null` if it hasn't been
 * generated yet (which happens before any browser has ever subscribed).
 */
export const getVapidPublicKey = async (
  actor: ActorSubclass<_SERVICE>,
): Promise<Uint8Array | null> => {
  const result = await actor.push_vapid_public_key();
  if (result.length === 0) return null;
  const raw = result[0];
  return raw instanceof Uint8Array ? raw : new Uint8Array(raw);
};

/**
 * Decodes a base64url string (as used by PushSubscription keys) to bytes.
 * `PushSubscription.toJSON()` returns base64url-encoded strings for `p256dh`
 * and `auth`; the canister expects raw bytes.
 */
const base64UrlToBytes = (b64url: string): Uint8Array => {
  // Restore standard base64 padding/chars.
  const padded = b64url.replace(/-/g, "+").replace(/_/g, "/");
  const padding =
    padded.length % 4 === 0 ? "" : "=".repeat(4 - (padded.length % 4));
  const binary = atob(padded + padding);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
};
