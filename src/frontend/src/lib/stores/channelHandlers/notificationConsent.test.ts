import { beforeEach, describe, expect, it, vi } from "vitest";
import { METHOD_NOT_FOUND_ERROR_CODE } from "$lib/utils/transport/utils";
import { get, type Writable } from "svelte/store";

const ORIGIN = "https://app.example.com";

vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    agentOptions: {},
    canisterId: Principal.fromText("rwlgt-iiaaa-aaaaa-aaaaa-cai"),
    backendCanisterConfig: { openid_configs: [] },
    frontendCanisterConfig: { related_origins: [], dev_csp: [] },
  };
});
vi.mock("$lib/state/featureFlags", async () => {
  const { writable } = await import("svelte/store");
  return { PUSH_NOTIFICATIONS: writable(true) };
});
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin: vi.fn(() => Promise.resolve({ result: "valid" })),
}));
/// The probe reads a service worker and an IndexedDB store this suite has neither
/// of. What it resolves to is this suite's subject: whether a question opens a
/// screen, and whether nothing to ask answers the app without one.
vi.mock("$lib/utils/notifications/notificationState", () => ({
  notificationsUnavailableHere: vi.fn(() => false),
  readBrowserPushState: vi.fn(() => Promise.resolve({})),
  readGranted: vi.fn(() => Promise.resolve(true)),
  resolveOptIn: vi.fn(() =>
    Promise.resolve({ screen: "enable", state: {}, consented: false }),
  ),
}));
vi.mock("$lib/stores/authentication.store", async () => {
  const { writable } = await import("svelte/store");
  return { authenticationStore: writable<unknown>(undefined) };
});

const setRequestOrigin = vi.fn();
vi.mock("$lib/stores/authorization.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    authorizationStore: {
      setRequestOrigin: (...args: unknown[]) => setRequestOrigin(...args),
    },
    authorizedStore: writable<unknown>(undefined),
    authorizationPromptStore: writable<{ prompt?: string }>({}),
  };
});

import {
  handleNotificationConsentRequest,
  NOTIFICATION_CONSENT_METHOD,
} from "./notificationConsent";
import {
  notificationConsentStore,
  type NotificationConsentContext,
} from "$lib/stores/notificationConsent.store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { INTERACTION_REQUIRED_ERROR_CODE } from "$lib/utils/transport/utils";
import { pendingScreenStore } from "$lib/stores/pendingScreen.store";
import { waitForStore } from "$lib/utils/utils";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
import {
  notificationsUnavailableHere,
  readBrowserPushState,
  readGranted,
  resolveOptIn,
} from "$lib/utils/notifications/notificationState";
import type { Channel, JsonRequest } from "$lib/utils/transport/utils";

const consentStatus = vi.fn(() => Promise.resolve(true));

const signIn = async () => {
  const { authorizedStore } = await import("$lib/stores/authorization.store");
  const { authenticationStore } =
    await import("$lib/stores/authentication.store");
  (authorizedStore as Writable<unknown>).set({
    accountNumberPromise: Promise.resolve(undefined),
  });
  (authenticationStore as unknown as Writable<unknown>).set({
    identityNumber: BigInt(10_000),
    actor: { notification_consent_granted: consentStatus },
  });
};

/** Runs one request to completion, settling the consent screen if it opens. */
const run = async ({
  params = {},
  omitParams = false,
  method = NOTIFICATION_CONSENT_METHOD,
  settle = true,
  origin = ORIGIN,
}: {
  params?: unknown;
  /** Send a request with no `params` member, as an app with nothing to pass does. */
  omitParams?: boolean;
  method?: string;
  settle?: boolean;
  origin?: string;
} = {}) => {
  const sent: Record<string, unknown>[] = [];
  const errors: string[] = [];
  const channel = {
    origin,
    send: (message: Record<string, unknown>) => {
      sent.push(message);
      return Promise.resolve();
    },
  } as unknown as Channel;

  const request = omitParams
    ? { jsonrpc: "2.0", id: 1, method }
    : { jsonrpc: "2.0", id: 1, method, params };
  const running = handleNotificationConsentRequest(channel, (error) =>
    errors.push(error),
  )(request as unknown as JsonRequest);

  if (settle) {
    void (async () => {
      await waitForStore(notificationConsentStore);
      notificationConsentStore.settle();
    })();
  }
  await running;
  return { sent, errors };
};

beforeEach(async () => {
  vi.clearAllMocks();
  consentStatus.mockResolvedValue(true);
  vi.mocked(validateDerivationOrigin).mockResolvedValue({ result: "valid" });
  vi.mocked(resolveOptIn).mockResolvedValue({
    screen: "enable",
    state: {} as never,
    consented: false,
  });
  vi.mocked(readGranted).mockResolvedValue(true);
  vi.mocked(notificationsUnavailableHere).mockReturnValue(false);
  notificationConsentStore.clear();
  (PUSH_NOTIFICATIONS as unknown as Writable<boolean>).set(true);
  const { authorizationPromptStore, authorizedStore } =
    await import("$lib/stores/authorization.store");
  (authorizationPromptStore as Writable<unknown>).set({});
  (authorizedStore as Writable<unknown>).set(undefined);
  await signIn();
});

describe("handleNotificationConsentRequest", () => {
  it("ignores another method", async () => {
    const { sent, errors } = await run({ method: "icrc34_delegation" });
    expect(sent).toEqual([]);
    expect(errors).toEqual([]);
  });

  /**
   * The canister refuses every notification endpoint while its own install argument is
   * off, so a ceremony offered here could only fail. The request is still answered:
   * dropped, it left the app waiting on a reply that was never coming, which hung the
   * sign-in it was part of. `icrc25_supported_standards` leaves the method out in this
   * state, so an app that asked was not told it could.
   */
  it("refuses the request while the feature is off", async () => {
    (PUSH_NOTIFICATIONS as unknown as Writable<boolean>).set(false);
    const { sent, errors } = await run({ settle: false });
    expect(sent).toHaveLength(1);
    expect(sent[0].error).toMatchObject({ code: METHOD_NOT_FOUND_ERROR_CODE });
    expect(errors).toEqual([]);
  });

  /** A silent request is refused the same way: what it asked for is absent here, which
   *  is not something showing the user a screen could have fixed. */
  it("refuses a silent request while the feature is off", async () => {
    (PUSH_NOTIFICATIONS as unknown as Writable<boolean>).set(false);
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    (authorizationPromptStore as Writable<unknown>).set({ prompt: "none" });

    const { sent } = await run({ settle: false });

    expect(sent[0].error).toMatchObject({ code: METHOD_NOT_FOUND_ERROR_CODE });
  });

  /** The identity switcher stays up during this screen. A switch used to leave the
   *  screen recording against the identity it opened for while the answer was read
   *  for the new one, so the ceremony has to start again. */
  it("starts the screen again for an identity switched to mid-ceremony", async () => {
    const { authenticationStore } =
      await import("$lib/stores/authentication.store");
    const switchedStatus = vi.fn(() => Promise.resolve(true));

    const sent: Record<string, unknown>[] = [];
    const channel = {
      origin: ORIGIN,
      send: (message: Record<string, unknown>) => {
        sent.push(message);
        return Promise.resolve();
      },
    } as unknown as Channel;
    const running = handleNotificationConsentRequest(channel, () => {})({
      jsonrpc: "2.0",
      id: 1,
      method: NOTIFICATION_CONSENT_METHOD,
      params: {},
    } as unknown as JsonRequest);

    const opened = await waitForStore(notificationConsentStore);
    expect(opened.identityNumber).toBe(BigInt(10_000));

    (authenticationStore as unknown as Writable<unknown>).set({
      identityNumber: BigInt(20_000),
      actor: { notification_consent_granted: switchedStatus },
    });

    const reopened = await waitForStore(notificationConsentStore, (context) =>
      context?.identityNumber === BigInt(20_000) ? context : undefined,
    );
    expect(reopened.identityNumber).toBe(BigInt(20_000));
    notificationConsentStore.settle();
    await running;

    // Read for the identity the user ended on, not the one the screen opened for.
    expect(readGranted).toHaveBeenCalledTimes(1);
    expect(readGranted).toHaveBeenCalledWith({
      identityNumber: BigInt(20_000),
      origin: ORIGIN,
      actor: { notification_consent_granted: switchedStatus },
    });
    expect(sent[0].result).toEqual({ granted: true });
  });

  /** The screen existed only to work out there was nothing to ask. Resolving that
   *  before the context is set is what keeps a spinner out of the sign-in. */
  it("answers without a screen where there is nothing to ask", async () => {
    vi.mocked(resolveOptIn).mockResolvedValue({
      screen: "skip",
      granted: true,
    });
    const opened = vi.fn();
    const unsubscribe = notificationConsentStore.subscribe((context) => {
      if (context !== undefined) {
        opened();
      }
    });

    const { sent, errors } = await run({ settle: false });

    unsubscribe();
    expect(opened).not.toHaveBeenCalled();
    expect(sent[0].result).toEqual({ granted: true });
    expect(errors).toEqual([]);
    // The answer is the one the resolution already read, not a second pair of reads.
    expect(readGranted).not.toHaveBeenCalled();
  });

  it("opens the screen on the state the resolution read", async () => {
    const state = {
      supported: true,
      permission: "denied" as NotificationPermission,
      subscribed: false,
      registered: false,
    };
    vi.mocked(resolveOptIn).mockResolvedValue({
      screen: "enable",
      state,
      consented: false,
    });
    let opened: NotificationConsentContext | undefined;
    const unsubscribe = notificationConsentStore.subscribe((context) => {
      opened ??= context;
    });

    const { sent } = await run();

    unsubscribe();
    // Carried over rather than read again, so answering only does what is left.
    expect(opened?.device).toBe(state);
    expect(opened?.consented).toBe(false);
    expect(sent[0].result).toEqual({ granted: true });
  });

  /** Authorizing is what replaces the screen the user is on, so this request has to
   *  own one from the moment it is accepted until it has asked: long enough that the
   *  flow keeps their screen instead of passing through a half-built one. */
  it("owns the screen from acceptance until the user has answered", async () => {
    const sent: Record<string, unknown>[] = [];
    const channel = {
      origin: ORIGIN,
      send: (message: Record<string, unknown>) => {
        sent.push(message);
        return Promise.resolve();
      },
    } as unknown as Channel;
    const running = handleNotificationConsentRequest(channel, () => {})({
      jsonrpc: "2.0",
      id: 1,
      method: NOTIFICATION_CONSENT_METHOD,
      params: {},
    } as unknown as JsonRequest);

    await waitForStore(notificationConsentStore);
    expect(get(pendingScreenStore)).toBe(true);

    notificationConsentStore.settle();
    await running;

    expect(get(pendingScreenStore)).toBe(false);
  });

  /** Reading the answer back takes two canister calls, and the screen the user came
   *  from is not what belongs in front of them while that runs — the redirect is. */
  it("releases the screen before reading the answer back", async () => {
    let answer: (granted: boolean) => void;
    vi.mocked(readGranted).mockReturnValueOnce(
      new Promise((resolve) => (answer = resolve)),
    );
    const sent: Record<string, unknown>[] = [];
    const channel = {
      origin: ORIGIN,
      send: (message: Record<string, unknown>) => {
        sent.push(message);
        return Promise.resolve();
      },
    } as unknown as Channel;
    const running = handleNotificationConsentRequest(channel, () => {})({
      jsonrpc: "2.0",
      id: 1,
      method: NOTIFICATION_CONSENT_METHOD,
      params: {},
    } as unknown as JsonRequest);

    await waitForStore(notificationConsentStore);
    notificationConsentStore.settle();
    await vi.waitFor(() => expect(readGranted).toHaveBeenCalled());

    expect(get(pendingScreenStore)).toBe(false);
    expect(sent).toEqual([]);

    answer!(true);
    await running;
    expect(sent[0].result).toEqual({ granted: true });
  });

  /** Nothing left to ask puts no screen in front of the user, so the hold it took on
   *  acceptance goes the moment it knows that. */
  it("releases the screen where there is nothing to ask", async () => {
    vi.mocked(resolveOptIn).mockResolvedValueOnce({
      screen: "skip",
      granted: true,
    });
    const { sent } = await run({ settle: false });
    expect(sent[0].result).toEqual({ granted: true });
    expect(get(pendingScreenStore)).toBe(false);
  });

  it("releases the screen for a request it refuses", async () => {
    vi.mocked(validateDerivationOrigin).mockResolvedValue({
      result: "invalid",
      message: "not a related origin",
    });
    const { errors } = await run({ settle: false });
    expect(errors).toEqual(["unverified-origin"]);
    expect(get(pendingScreenStore)).toBe(false);
  });

  /** The probe is taken when a request is accepted, so a ceremony that runs before
   *  this one's turn can subscribe the browser it found bare. Asking from that
   *  snapshot would offer to set up a device that is already set up. */
  it("probes again where a ceremony ran between the probe and its turn", async () => {
    const channel = {
      origin: ORIGIN,
      send: () => Promise.resolve(),
    } as unknown as Channel;
    const ask = (id: number) =>
      handleNotificationConsentRequest(channel, () => {})({
        jsonrpc: "2.0",
        id,
        method: NOTIFICATION_CONSENT_METHOD,
        params: {},
      } as unknown as JsonRequest);

    // Both accepted, so both probe, before either holds the queue.
    const first = ask(1);
    const second = ask(2);
    expect(readBrowserPushState).toHaveBeenCalledTimes(2);

    // The first holds the queue. It reused its own probe, taken a moment earlier.
    await waitForStore(notificationConsentStore);
    notificationConsentStore.settle();
    await first;
    expect(readBrowserPushState).toHaveBeenCalledTimes(2);

    // The second's turn, with its probe now predating a ceremony.
    await waitForStore(notificationConsentStore);
    notificationConsentStore.settle();
    await second;
    expect(readBrowserPushState).toHaveBeenCalledTimes(3);
  });

  /** iOS delivers notifications only to a Home Screen app, which is not built, so
   *  nothing is offered there. The method exists, so this is an answer and not a
   *  refusal: the app is told plainly that it may not notify here. */
  it("answers no on iOS without opening a screen", async () => {
    vi.mocked(notificationsUnavailableHere).mockReturnValue(true);
    const opened = vi.fn();
    const unsubscribe = notificationConsentStore.subscribe((context) => {
      if (context !== undefined) {
        opened();
      }
    });

    const { sent, errors } = await run({ settle: false });

    unsubscribe();
    expect(opened).not.toHaveBeenCalled();
    expect(sent[0].result).toEqual({ granted: false });
    expect(errors).toEqual([]);
    expect(resolveOptIn).not.toHaveBeenCalled();
    expect(readGranted).not.toHaveBeenCalled();
  });

  it("reports what the canister recorded", async () => {
    const { sent } = await run();
    expect(sent).toHaveLength(1);
    expect(sent[0].result).toEqual({ granted: true });
    expect(readGranted).toHaveBeenCalledWith({
      identityNumber: BigInt(10_000),
      origin: ORIGIN,
      actor: { notification_consent_granted: consentStatus },
    });
  });

  it("accepts a request that carries no params", async () => {
    const { sent, errors } = await run({ omitParams: true });
    expect(sent[0].result).toEqual({ granted: true });
    expect(errors).toEqual([]);
  });

  it("reports a refusal rather than failing", async () => {
    vi.mocked(readGranted).mockResolvedValue(false);
    const { sent, errors } = await run();
    expect(sent[0].result).toEqual({ granted: false });
    expect(errors).toEqual([]);
  });

  /** Consent is the user's answer, not a cached artifact, so a request that may not
   *  paint is refused before the ceremony starts. */
  it("silent requests never paint", async () => {
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    (authorizationPromptStore as Writable<unknown>).set({ prompt: "none" });

    const { sent, errors } = await run({ settle: false });

    expect(sent).toHaveLength(1);
    expect(sent[0].error).toMatchObject({
      code: INTERACTION_REQUIRED_ERROR_CODE,
    });
    expect(setRequestOrigin).not.toHaveBeenCalled();
    expect(errors).toEqual([]);
  });

  it("a malformed request never paints either", async () => {
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    (authorizationPromptStore as Writable<unknown>).set({ prompt: "none" });

    const { sent, errors } = await run({
      params: { icrc95DerivationOrigin: 42 },
      settle: false,
    });

    expect(sent[0].error).toBeDefined();
    expect(errors).toEqual([]);
  });

  it("refuses an unverified derivation origin without answering", async () => {
    vi.mocked(validateDerivationOrigin).mockResolvedValue({
      result: "invalid",
      message: "not a related origin",
    });
    const { sent, errors } = await run({ settle: false });
    expect(sent).toEqual([]);
    expect(errors).toEqual(["unverified-origin"]);
  });
});
