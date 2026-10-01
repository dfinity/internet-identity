import { createServer } from "node:http";
import { expect, type BrowserContext, type Page } from "@playwright/test";

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

/** Where the stub keeps what a real browser would keep for the origin, so a window
 *  opened later finds the subscription and the permission an earlier one left. */
const STATE_KEY = "ii-e2e-push-state";

/** How many times II asked for the permission, read back by the scenarios that turn
 *  on the one thing a prompt cannot fix. */
export const promptCount = (page: Page): Promise<number> =>
  page.evaluate(
    () =>
      (window as unknown as { __iiPromptCount?: number }).__iiPromptCount ?? 0,
  );

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
    ({ endpoint, initialPermission, stateKey }) => {
      // A real browser keeps both for the origin, not for the page, and the ceremony
      // runs in a window opened after the scenario starts. Kept where that window
      // finds them, so "this browser is already set up" is reachable.
      const stored = ((): { permission?: string; endpoint?: string } => {
        try {
          return JSON.parse(window.localStorage.getItem(stateKey) ?? "{}");
        } catch {
          return {};
        }
      })();
      let permission: NotificationPermission = (stored.permission ??
        initialPermission) as NotificationPermission;
      let subscription: { endpoint: string } | null =
        stored.endpoint === undefined ? null : { endpoint: stored.endpoint };

      const remember = () => {
        try {
          window.localStorage.setItem(
            stateKey,
            JSON.stringify({ permission, endpoint: subscription?.endpoint }),
          );
        } catch {
          // Locked storage only costs the scenario its memory of this browser.
        }
      };

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
              remember();
              return Promise.resolve(true);
            },
          } as unknown as { endpoint: string };
          remember();
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
            const counted = window as unknown as { __iiPromptCount?: number };
            counted.__iiPromptCount = (counted.__iiPromptCount ?? 0) + 1;
            // A browser only prompts from `default`; a refusal stands until the
            // user changes it in browser settings, which no prompt can do.
            if (permission === "default") {
              permission = "granted";
            }
            remember();
            return Promise.resolve(permission);
          },
        },
      });
    },
    {
      endpoint: RELAY_ENDPOINT,
      initialPermission: options.permission ?? "default",
      stateKey: STATE_KEY,
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

/**
 * A push service, on this machine.
 *
 * Headless Chromium ships the Push API with no push service behind it, so a real
 * `subscribe` never resolves. This stands in for one: the browser is handed this
 * server's URL as its endpoint, the canister's dispatch posts a wake-up to it for
 * real over an HTTP outcall, and the caller turns each receipt into a push
 * delivered to the worker — the one hop a test has to bridge.
 *
 * The deployment must carry `notifications_allow_insecure_endpoint`, since a local
 * server has no certificate to offer.
 */
export interface PushRelay {
  endpoint: string;
  /** Resolves once the canister has posted at least `count` wake-ups. */
  waitForWakeUps: (count: number) => Promise<void>;
  received: () => number;
  close: () => Promise<void>;
}

/// A fixed port, because the canister keeps the endpoint a browser registered:
/// an ephemeral one leaves every stored subscription pointing at a relay that no
/// longer exists, and its wake-up is refused rather than delivered.
const RELAY_PORT = 11190;

export const startPushRelay = async (): Promise<PushRelay> => {
  let received = 0;
  const server = createServer((request, response) => {
    request.resume();
    request.on("end", () => {
      received += 1;
      // A relay answers 201 with nothing in it.
      response.writeHead(201).end();
    });
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(RELAY_PORT, "127.0.0.1", () => resolve());
  });

  return {
    endpoint: `http://127.0.0.1:${RELAY_PORT}/push`,
    received: () => received,
    waitForWakeUps: async (count) => {
      await expect
        .poll(() => received, {
          message: `the canister sent fewer than ${count} wake-up(s)`,
          timeout: 30_000,
        })
        .toBeGreaterThanOrEqual(count);
    },
    close: () => new Promise<void>((resolve) => server.close(() => resolve())),
  };
};

/**
 * Makes a browser push-capable without taking its service worker away.
 *
 * Only `subscribe` is stubbed, to hand the canister an endpoint it can reach:
 * everything else is the browser's own, so the worker registers, installs and
 * handles what it is given.
 */
export const armRealWorkerPush = async (
  context: BrowserContext,
  origin: string,
  endpoint: string,
): Promise<void> => {
  await context.grantPermissions(["notifications"], { origin });

  await context.addInitScript((endpoint) => {
    let subscription: unknown = null;
    const fake = {
      endpoint,
      options: { userVisibleOnly: true },
      unsubscribe: () => {
        subscription = null;
        return Promise.resolve(true);
      },
      getKey: () => null,
      toJSON: () => ({ endpoint }),
    };
    PushManager.prototype.subscribe = () => {
      subscription = fake;
      return Promise.resolve(fake as unknown as PushSubscription);
    };
    PushManager.prototype.getSubscription = () =>
      Promise.resolve(subscription as PushSubscription | null);
  }, endpoint);

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

/**
 * Delivers a push to the worker registered for `origin`, the way the browser would
 * on hearing from a push service. This is what DevTools' own Push button does, and
 * it is the only way to reach a worker in a browser with no push service.
 */
export const deliverPush = async (
  page: Page,
  origin: string,
): Promise<void> => {
  const session = await page.context().newCDPSession(page);
  const registrations: { registrationId: string; scopeURL: string }[] = [];
  session.on(
    "ServiceWorker.workerRegistrationUpdated",
    ({ registrations: updated }) => {
      registrations.push(...updated);
    },
  );
  await session.send("ServiceWorker.enable");

  await expect
    .poll(() => registrations.some((one) => one.scopeURL.startsWith(origin)), {
      message: `no service worker registered for ${origin}`,
    })
    .toBe(true);
  const registration = registrations.find((one) =>
    one.scopeURL.startsWith(origin),
  );

  await session.send("ServiceWorker.deliverPushMessage", {
    origin,
    registrationId: registration?.registrationId ?? "",
    // A wake-up carries nothing: what it is for is asked of the canister.
    data: "",
  });
  await session.detach();
};
