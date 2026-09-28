// Browser-side Web Push subscription: register the push service worker, ask for
// permission, and subscribe with the VAPID public key. The relay binds the subscription
// to that key and accepts only pushes carrying a JWT signed by its private half (see
// vapidPool).
//
// The subscription's own p256dh and auth keys are ignored, since the push carries no
// body to encrypt.

import { bufFromBufLike } from "$lib/utils/utils";
import { readCanisterId } from "$lib/utils/init";
import { agentOptions } from "$lib/globals";

// SvelteKit bundles the worker here; it is registered on opt-in rather than on
// every load (kit.serviceWorker.register is off).
//
// The canister id and whether to fetch the root key ride on the URL: a woken worker
// has no page to ask and no document to read them from, and the registration keeps the
// URL it was made with. Without the second one, every call a worker makes against a
// local replica fails certificate verification.
const serviceWorkerUrl = (): string => {
  const params = new URLSearchParams({ canisterId: readCanisterId() });
  if (agentOptions.shouldFetchRootKey === true) {
    params.set("fetchRootKey", "1");
  }
  return `/service-worker.js?${params.toString()}`;
};

export const isPushSupported = (): boolean =>
  "serviceWorker" in navigator &&
  "PushManager" in window &&
  "Notification" in window;

/** Prompts for notification permission, and answers what the user chose. A prompt
 *  closed without an answer leaves it at `default`, which can be asked again. */
export const requestNotificationPermission =
  (): Promise<NotificationPermission> => Notification.requestPermission();

const registerServiceWorker = async (): Promise<ServiceWorkerRegistration> => {
  await navigator.serviceWorker.register(serviceWorkerUrl(), {
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
