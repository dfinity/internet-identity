// Browser-side Web Push subscription: register the push service worker, ask
// for permission, and subscribe with the VAPID public key. The relay binds the
// subscription to that key and later accepts only pushes carrying a JWT signed
// by the matching private key (see vapidPool).

import { bufFromBufLike } from "$lib/utils/utils";

// SvelteKit bundles the worker here; it is registered on opt-in rather than on
// every load (kit.serviceWorker.register is off).
const SERVICE_WORKER_URL = "/service-worker.js";

export interface PushSubscriptionKeys {
  endpoint: string;
  /** Client public key, 65 bytes uncompressed. */
  p256dh: Uint8Array;
  /** Client auth secret, 16 bytes. */
  auth: Uint8Array;
}

export const isPushSupported = (): boolean =>
  "serviceWorker" in navigator &&
  "PushManager" in window &&
  "Notification" in window;

/** Prompts for notification permission; resolves true only on an explicit grant. */
export const requestNotificationPermission = async (): Promise<boolean> =>
  (await Notification.requestPermission()) === "granted";

const registerServiceWorker = async (): Promise<ServiceWorkerRegistration> => {
  await navigator.serviceWorker.register(SERVICE_WORKER_URL, {
    type: "module",
  });
  return navigator.serviceWorker.ready;
};

/**
 * Subscribes this device to `applicationServerKey`. Any prior subscription is
 * dropped first: it is bound to an old VAPID key whose private half we no longer
 * hold, so its endpoint would reject every push we could sign.
 */
export const subscribeToPush = async (
  applicationServerKey: Uint8Array,
): Promise<PushSubscriptionKeys> => {
  const registration = await registerServiceWorker();
  await (await registration.pushManager.getSubscription())?.unsubscribe();
  const subscription = await registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: bufFromBufLike(applicationServerKey),
  });

  const p256dh = subscription.getKey("p256dh");
  const auth = subscription.getKey("auth");
  if (p256dh === null || auth === null) {
    throw new Error("Push subscription is missing its encryption keys");
  }
  return {
    endpoint: subscription.endpoint,
    p256dh: new Uint8Array(p256dh),
    auth: new Uint8Array(auth),
  };
};

/** `scheme://host[:port]` of a relay endpoint — the JWT `aud` the pool signs for. */
export const relayOriginOf = (endpoint: string): string =>
  new URL(endpoint).origin;
