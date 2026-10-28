import { createServer } from "node:http";
import { createDecipheriv, createECDH, createHmac } from "node:crypto";
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

/**
 * The subscription keys a browser would have minted. Fixed rather than generated, so the
 * page stub and the relay below can both name them without passing anything between.
 *
 * The canister seals a wake-up's payload against these, so the relay can open one the way
 * the browser would, and what the worker is handed is what the canister actually sent.
 */
const SUBSCRIBER_PRIVATE_KEY = Buffer.alloc(32, 7);
const AUTH_SECRET = Buffer.alloc(16, 3);

const subscriberKeys = (): { publicKey: Buffer; privateKey: Buffer } => {
  const ecdh = createECDH("prime256v1");
  ecdh.setPrivateKey(SUBSCRIBER_PRIVATE_KEY);
  return { publicKey: ecdh.getPublicKey(), privateKey: SUBSCRIBER_PRIVATE_KEY };
};

/** HKDF as RFC 8291 and RFC 8188 use it: one extract, one expand of a single block. */
const hkdf = (
  salt: Buffer,
  ikm: Buffer,
  info: Buffer,
  length: number,
): Buffer => {
  const prk = createHmac("sha256", salt).update(ikm).digest();
  return createHmac("sha256", prk)
    .update(Buffer.concat([info, Buffer.from([1])]))
    .digest()
    .subarray(0, length);
};

/**
 * Opens a sealed wake-up the way the browser does before waking the worker: the header
 * says which salt and ephemeral key it was sealed under, and the rest follows from the
 * subscription's own keys.
 */
const openPayload = (body: Buffer): string => {
  const salt = body.subarray(0, 16);
  const keyIdLength = body[20];
  const keyId = body.subarray(21, 21 + keyIdLength);
  const ciphertext = body.subarray(21 + keyIdLength);

  const ecdh = createECDH("prime256v1");
  ecdh.setPrivateKey(SUBSCRIBER_PRIVATE_KEY);
  const shared = ecdh.computeSecret(keyId);

  const ikm = hkdf(
    AUTH_SECRET,
    shared,
    Buffer.concat([
      Buffer.from("WebPush: info\0", "utf8"),
      subscriberKeys().publicKey,
      keyId,
    ]),
    32,
  );
  const cek = hkdf(
    salt,
    ikm,
    Buffer.from("Content-Encoding: aes128gcm\0", "utf8"),
    16,
  );
  const nonce = hkdf(
    salt,
    ikm,
    Buffer.from("Content-Encoding: nonce\0", "utf8"),
    12,
  );

  const tag = ciphertext.subarray(ciphertext.length - 16);
  const decipher = createDecipheriv("aes-128-gcm", cek, nonce);
  decipher.setAuthTag(tag);
  const record = Buffer.concat([
    decipher.update(ciphertext.subarray(0, ciphertext.length - 16)),
    decipher.final(),
  ]);
  // RFC 8188 ends the last record's data with a delimiter octet.
  return record.subarray(0, record.length - 1).toString("utf8");
};

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
 * Lifts a refusal the way browser settings do, which no prompt can. Nothing in the
 * page triggers it, so a screen that carries on afterwards did so by watching.
 *
 * Where it lands differs by browser, and so does what the screen may then do:
 * Chrome's switch grants outright, while Firefox's route is to clear the block,
 * which returns the permission to `default` and leaves a prompt still to be raised.
 */
export const liftPermissionInSettings = (
  page: Page,
  to: "granted" | "default" = "granted",
): Promise<void> =>
  page.evaluate(
    (permission) =>
      (
        window as unknown as {
          __iiLiftPermission: (to: string) => void;
        }
      ).__iiLiftPermission(permission),
    to,
  );

/**
 * Turns the push feature flag on for every page this context opens.
 *
 * Off by default, so every scenario that wants the feature says so. The key shape is
 * `LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name` from `featureFlags.ts`, spelled out so a
 * changed prefix fails loudly instead of running with the feature off.
 */
const armTheFeatureFlag = async (context: BrowserContext): Promise<void> => {
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
 * Installs the stub and turns the feature flag on, for every page this context opens.
 * Context-wide rather than page-wide, because the ceremony happens in a window the test
 * app opens after the scenario starts.
 */
export const armPushNotifications = async (
  context: BrowserContext,
  options: PushBrowserOptions = {},
): Promise<void> => {
  await context.addInitScript(
    ({ endpoint, initialPermission, stateKey, p256dh, auth }) => {
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
            // A registration derives what seals its payloads from these, so a stub
            // without them is one no identity can register from.
            getKey: (name: string) =>
              name === "auth"
                ? new Uint8Array(auth).buffer
                : new Uint8Array(p256dh).buffer,
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
      // What a test uses in place of the browser's settings UI, which it cannot
      // reach. Only the setting changes: nothing here notifies the page.
      Object.defineProperty(window, "__iiLiftPermission", {
        configurable: true,
        value: (to: string) => {
          permission = to as NotificationPermission;
          remember();
        },
      });
    },
    {
      endpoint: RELAY_ENDPOINT,
      initialPermission: options.permission ?? "default",
      stateKey: STATE_KEY,
      p256dh: [...subscriberKeys().publicKey],
      auth: [...AUTH_SECRET],
    },
  );

  await armTheFeatureFlag(context);
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
  /** What the wake-ups carried, opened with the subscription's keys, in order. */
  payloads: () => string[];
  /** The paths the canister posted wake-ups to, in order.
   *
   *  A browser registers the endpoint it was handed, so posting to one path rather than
   *  another is the canister having been told a new endpoint. That is the only way from
   *  out here to tell a launch that registered from one that merely looked right. */
  receivedPaths: () => string[];
  /**
   * Answers every later wake-up the way a push service answers one for a subscription
   * that no longer exists.
   *
   * The one thing that tells Internet Identity a Home Screen app is gone: iOS destroys
   * the app's storage when the user deletes it and tells nobody, so the push coming
   * back dead is the only evidence there is.
   */
  reportGone: (gone: boolean) => void;
  close: () => Promise<void>;
}

/// Fixed for the life of a worker, and a worker's own. Fixed, because the canister keeps
/// the endpoint a browser registered: an ephemeral one leaves every stored subscription
/// pointing at a relay that no longer exists, and its wake-up is refused rather than
/// delivered. A worker's own, because the scenarios that take a relay run in parallel off
/// CI, and two of them on one port is `EADDRINUSE` rather than a test failure anyone can
/// read. Nothing outside the suite names this port.
const RELAY_PORT = 11190 + Number(process.env.TEST_PARALLEL_INDEX ?? 0);

export const startPushRelay = async (): Promise<PushRelay> => {
  let received = 0;
  let gone = false;
  const paths: string[] = [];
  const payloads: string[] = [];
  const server = createServer((request, response) => {
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => chunks.push(chunk));
    request.on("end", () => {
      received += 1;
      paths.push(request.url ?? "");
      if (chunks.length > 0) {
        payloads.push(openPayload(Buffer.concat(chunks)));
      }
      if (gone) {
        // What a push service answers for a subscription that no longer exists.
        response.writeHead(410).end();
        return;
      }
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
    receivedPaths: () => [...paths],
    payloads: () => [...payloads],
    reportGone: (report: boolean) => {
      gone = report;
    },
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

  const keys = {
    endpoint,
    p256dh: [...subscriberKeys().publicKey],
    auth: [...AUTH_SECRET],
  };
  await context.addInitScript((keys) => {
    const { endpoint, p256dh, auth } = keys;
    let subscription: unknown = null;
    const fake = {
      endpoint,
      options: { userVisibleOnly: true },
      unsubscribe: () => {
        subscription = null;
        return Promise.resolve(true);
      },
      // What the canister seals a payload against, as a real subscription hands them over.
      getKey: (name: string) =>
        name === "auth"
          ? new Uint8Array(auth).buffer
          : new Uint8Array(p256dh).buffer,
      toJSON: () => ({ endpoint }),
    };
    PushManager.prototype.subscribe = () => {
      subscription = fake;
      return Promise.resolve(fake as unknown as PushSubscription);
    };
    PushManager.prototype.getSubscription = () =>
      Promise.resolve(subscription as PushSubscription | null);
  }, keys);

  await armTheFeatureFlag(context);
};

/**
 * Delivers a push to the worker registered for `origin`, the way the browser would
 * on hearing from a push service. This is what DevTools' own Push button does, and
 * it is the only way to reach a worker in a browser with no push service.
 */
export const deliverPush = async (
  page: Page,
  origin: string,
  /** What the wake-up carried. The relay hands this over opened, as the browser would. */
  data: string,
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
    data,
  });
  await session.detach();
};

/** How much of each relay's payloads {@link deliverNewPushes} has handed over. */
const alreadyDelivered = new WeakMap<PushRelay, number>();

/**
 * Hands the worker every wake-up the relay has taken and not been asked to deliver yet.
 *
 * A push service delivers each wake-up it is posted, and only the one naming an entry can
 * show that entry. Which of the ones a relay holds that is cannot be worked out from
 * here: the canister posts wake-ups of its own whenever an identity is pointed at an app,
 * and the relay listens on one port for every scenario, so it also takes in wake-ups for
 * subscriptions earlier scenarios registered. Singling one out by its position among
 * those hands over a wake-up for something else and leaves the awaited one undelivered.
 */
export const deliverNewPushes = async (
  page: Page,
  origin: string,
  relay: PushRelay,
): Promise<void> => {
  const posted = relay.payloads();
  for (const payload of posted.slice(alreadyDelivered.get(relay) ?? 0)) {
    await deliverPush(page, origin, payload);
  }
  alreadyDelivered.set(relay, posted.length);
};

/** What the worker registered in this context has on screen, as the browser holds it. */
export const shownByTheWorker = async (
  context: BrowserContext,
): Promise<{ title: string; body: string }[]> => {
  const [worker] = context.serviceWorkers();
  if (worker === undefined) {
    return [];
  }
  return await worker.evaluate(async () => {
    const scope = self as unknown as ServiceWorkerGlobalScope;
    const shown = await scope.registration.getNotifications();
    return shown.map((one) => ({ title: one.title, body: one.body }));
  });
};
