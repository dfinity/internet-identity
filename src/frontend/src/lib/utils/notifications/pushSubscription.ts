// Browser-side Web Push subscription: register the push service worker, ask for
// permission, and subscribe with the VAPID public key. The relay binds the subscription
// to that key and accepts only pushes carrying a JWT signed by its private half (see
// vapidPool).
//
// The subscription's own p256dh and auth keys are ignored, since the push carries no
// body to encrypt.

import { bufFromBufLike } from "$lib/utils/utils";

// SvelteKit bundles the worker here; it is registered on opt-in rather than on
// every load (kit.serviceWorker.register is off).
const SERVICE_WORKER_URL = "/service-worker.js";

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
): Promise<string> => {
  const registration = await registerServiceWorker();
  await (await registration.pushManager.getSubscription())?.unsubscribe();
  // `userVisibleOnly` is mandatory, and obliges the worker to show something per push.
  const subscription = await registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: bufFromBufLike(applicationServerKey),
  });
  return subscription.endpoint;
};

/** This browser's current push subscription, or `undefined` if not subscribed. */
export const currentDeviceSubscription = async (): Promise<
  PushSubscription | undefined
> => {
  const registration = await navigator.serviceWorker.getRegistration();
  return (await registration?.pushManager.getSubscription()) ?? undefined;
};

/** `scheme://host[:port]` of a relay endpoint — the JWT `aud` the pool signs for. */
export const relayOriginOf = (endpoint: string): string =>
  new URL(endpoint).origin;
