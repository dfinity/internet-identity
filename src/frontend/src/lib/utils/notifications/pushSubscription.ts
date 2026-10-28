// Browser-side Web Push subscription: register the push service worker, ask for
// permission, and subscribe with Internet Identity's own VAPID key. The relay binds the
// subscription to that key and accepts only pushes carrying a JWT signed by its private
// half, which is a threshold key the canister holds.
//
// The subscription's own p256dh and auth keys are what a wake-up's payload is sealed
// against; see payloadKeys.

import { bufFromBufLike } from "$lib/utils/utils";
import { agentOptions, canisterId } from "$lib/globals";
import { workerConfigUrl } from "./workerConfig";

// Registered on opt-in rather than on every load (kit.serviceWorker.register is off).
const serviceWorkerUrl = (): string =>
  workerConfigUrl({ canisterId: canisterId.toText(), agentOptions });

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
): Promise<PushSubscription> => {
  const registration = await registerServiceWorker();
  await (await registration.pushManager.getSubscription())?.unsubscribe();
  // `userVisibleOnly` is mandatory, and obliges the worker to show something per push.
  return registration.pushManager.subscribe({
    userVisibleOnly: true,
    applicationServerKey: bufFromBufLike(applicationServerKey),
  });
};

/** This browser's current push subscription, or `undefined` if not subscribed. */
export const currentDeviceSubscription = async (): Promise<
  PushSubscription | undefined
> => {
  const registration = await navigator.serviceWorker.getRegistration();
  return (await registration?.pushManager.getSubscription()) ?? undefined;
};

/** `scheme://host[:port]` of a relay endpoint, which is what a push is addressed to. */
export const relayOriginOf = (endpoint: string): string =>
  new URL(endpoint).origin;
