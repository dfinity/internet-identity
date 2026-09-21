import type { BrowserContext } from "@playwright/test";

/**
 * Makes a browser look push-capable.
 *
 * Headless Chromium ships the Push API but has no push service behind it, so
 * `pushManager.subscribe` never resolves. `Notification`, `navigator.serviceWorker`
 * and the push manager are all replaced here, so nothing downstream covers the native
 * permission prompt or a real service-worker registration. What does run for real is
 * II's side: the consent screen, the VAPID pool the browser signs and the rows the
 * canister writes.
 *
 * Asserts nothing about the endpoint, which is a relay URL nothing is ever sent to.
 */

/** Obviously not a relay, so a push that escaped the stub would fail loudly. */
const RELAY_ENDPOINT = "https://push.e2e.invalid/subscription";

export interface PushBrowserOptions {
  /**
   * Where the permission starts. `"default"` is the only state that reaches the prompt;
   * `"denied"` is one II has to recognise without prompting, since it cannot.
   */
  permission?: "default" | "denied";
}

/**
 * Installs the stub and turns the feature flag on, for every page this context opens.
 * Context-wide rather than page-wide, because the ceremony happens in a window the test
 * app opens after the scenario starts.
 */
export const armPushNotifications = async (
  context: BrowserContext,
  options: PushBrowserOptions = {},
): Promise<void> => {
  await context.addInitScript(
    ({ endpoint, initialPermission }) => {
      let permission: NotificationPermission =
        initialPermission as NotificationPermission;
      let subscription: { endpoint: string } | null = null;

      const pushManager = {
        getSubscription: () => Promise.resolve(subscription),
        subscribe: () => {
          // The real one rejects rather than resolving unsubscribed, and II
          // treats the rejection as the user refusing.
          if (permission !== "granted") {
            return Promise.reject(
              new Error("push subscribe without permission"),
            );
          }
          subscription = {
            endpoint,
            unsubscribe: () => {
              subscription = null;
              return Promise.resolve(true);
            },
          } as unknown as { endpoint: string };
          return Promise.resolve(subscription);
        },
      };
      const registration = {
        pushManager,
        scope: "/",
        unregister: () => Promise.resolve(true),
      };

      Object.defineProperty(navigator, "serviceWorker", {
        configurable: true,
        value: {
          register: () => Promise.resolve(registration),
          getRegistration: () => Promise.resolve(registration),
          ready: Promise.resolve(registration),
          addEventListener: () => {},
        },
      });
      // `isPushSupported` tests for the constructor, not for an instance.
      Object.defineProperty(window, "PushManager", {
        configurable: true,
        value: class PushManager {},
      });
      Object.defineProperty(window, "Notification", {
        configurable: true,
        value: {
          get permission() {
            return permission;
          },
          requestPermission: () => {
            // A browser only prompts from `default`; a refusal stands until the
            // user changes it in browser settings, which no prompt can do.
            if (permission === "default") {
              permission = "granted";
            }
            return Promise.resolve(permission);
          },
        },
      });
    },
    {
      endpoint: RELAY_ENDPOINT,
      initialPermission: options.permission ?? "default",
    },
  );

  // Off by default, so every scenario that wants the feature says so. The key shape is
  // `LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name` from `featureFlags.ts`, spelled out so a
  // changed prefix fails loudly instead of running with the feature off.
  await context.addInitScript(() => {
    try {
      window.localStorage.setItem(
        "ii-localstorage-feature-flags__PUSH_NOTIFICATIONS",
        JSON.stringify(true),
      );
    } catch {
      // localStorage may be locked in some test contexts.
    }
  });
};
