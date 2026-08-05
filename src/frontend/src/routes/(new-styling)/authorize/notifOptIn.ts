/**
 * Whether to show the notifications opt-in screen after Continue.
 *
 * The opt-in is a one-time ask, not something the user re-answers on every
 * sign-in. The answer is remembered per `(identity, origin)`:
 *
 * - per identity, because enabling notifications is per identity and never per
 *   device — two identities sharing a browser each decide for themselves;
 * - per origin, because consent is granted per dApp, so a new dApp asking is
 *   legitimate while the same dApp asking twice is not.
 *
 * It is remembered in `localStorage`, i.e. per browser profile, which lines up
 * with what the answer actually commits to: a push subscription belongs to one
 * browser. Signing in on a second device has no subscription yet, so being
 * asked there is correct rather than a bug.
 */

type Decision = "enabled" | "dismissed";

const storageKey = (identityNumber: bigint, origin: string): string =>
  `ii-push-optin:${identityNumber}:${origin}`;

/**
 * Whether this context can create a push subscription **right now**.
 *
 * `Notification` is checked separately rather than assumed from `PushManager`:
 * iOS exposes them independently, and every caller reads
 * `Notification.permission` immediately after this guard.
 */
export const pushCapable = (): boolean =>
  typeof navigator !== "undefined" &&
  "serviceWorker" in navigator &&
  typeof window !== "undefined" &&
  "PushManager" in window &&
  "Notification" in window;

/**
 * iOS/iPadOS, where Web Push exists **only** inside a home-screen PWA — so a
 * Safari tab (which is where a dApp's redirect sign-in lands) cannot subscribe,
 * yet the identity can still grant consent now and subscribe later from II's own
 * installed app. This is the case that makes consent and subscription separable:
 * consent is a canister call that works anywhere; subscription needs the
 * standalone context.
 *
 * iPadOS reports itself as a Mac, so the touch-point check catches it.
 */
export const applePushDeferred = (): boolean => {
  if (pushCapable()) return false;
  if (typeof navigator === "undefined") return false;
  const ua = navigator.userAgent ?? "";
  const iPhoneOrPad = /iP(hone|od|ad)/.test(ua);
  const iPadAsMac =
    navigator.platform === "MacIntel" && (navigator.maxTouchPoints ?? 0) > 1;
  return iPhoneOrPad || iPadAsMac;
};

/**
 * Reads a stored decision. A `localStorage` failure (Safari private mode,
 * storage disabled) is reported as "already decided" so the screen is skipped:
 * if the answer cannot be persisted, asking would repeat on every single
 * sign-in with no way for the user to stop it — the exact behaviour this
 * module exists to prevent. Losing the prompt is the lesser fault.
 */
const decisionFor = (
  identityNumber: bigint,
  origin: string,
): Decision | "unknown" | "unavailable" => {
  try {
    const stored = localStorage.getItem(storageKey(identityNumber, origin));
    if (stored === "enabled" || stored === "dismissed") {
      return stored;
    }
    return "unknown";
  } catch {
    return "unavailable";
  }
};

/** Records the user's answer so they are not asked again for this pair. */
export const recordNotifOptInDecision = (
  identityNumber: bigint,
  origin: string,
  decision: Decision,
): void => {
  try {
    localStorage.setItem(storageKey(identityNumber, origin), decision);
  } catch {
    // Nothing to do: the prompt is a nicety, and a browser that refuses
    // storage is one where push is unlikely to work anyway.
  }
};

/**
 * Whether this identity has notifications on for `origin`.
 *
 * Reads the same record the opt-in screen writes, so the Continue screen can
 * show the state without a canister round trip on a latency-sensitive path.
 * False when the browser permission has since been revoked, because the stored
 * "enabled" only ever meant "the user said yes here" — the browser is the
 * authority on whether anything can actually be delivered.
 */
export const notificationsEnabledFor = (
  identityNumber: bigint,
  origin: string,
): boolean =>
  notificationsGloballyGranted() &&
  decisionFor(identityNumber, origin) === "enabled";

/**
 * Whether the **global** layer is already in place: the browser-level
 * notification permission, which belongs to II's origin and is therefore
 * granted once and shared by every dApp.
 *
 * This is what distinguishes the two asks. Before it is granted, the opt-in
 * screen is a first-run screen that explains notifications and triggers the
 * browser's own prompt. After it is granted, no browser prompt can appear again,
 * so the remaining question is purely II's own: may *this app* reach you.
 */
export const notificationsGloballyGranted = (): boolean =>
  pushCapable() && Notification.permission === "granted";

/**
 * Whether to show the opt-in screen for this identity and origin.
 *
 * Offered when this context can subscribe **or** when it is an iOS tab that can
 * grant consent now and subscribe later — see {@link applePushDeferred}. Skipped
 * when the user already answered, when neither is true, and when a subscribe-here
 * context has permission `denied` (which cannot be re-requested from script, so
 * the primary action would silently do nothing). The deferred path has no browser
 * prompt to be denied, so that guard does not apply to it.
 */
export const shouldOfferNotifications = (
  identityNumber: bigint,
  origin: string,
): boolean => {
  if (!pushCapable() && !applePushDeferred()) {
    return false;
  }
  if (pushCapable() && Notification.permission === "denied") {
    return false;
  }
  return decisionFor(identityNumber, origin) === "unknown";
};
