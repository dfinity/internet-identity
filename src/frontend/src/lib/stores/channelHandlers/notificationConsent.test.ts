import { beforeEach, describe, expect, it, vi } from "vitest";
import type { Writable } from "svelte/store";

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
vi.mock("$lib/stores/authentication.store", async () => {
  const { writable } = await import("svelte/store");
  return { authenticationStore: writable<unknown>(undefined) };
});

const setRequestContext = vi.fn();
vi.mock("$lib/stores/authorization.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    authorizationStore: {
      setRequestContext: (...args: unknown[]) => setRequestContext(...args),
    },
    authorizedStore: writable<unknown>(undefined),
    authorizationPromptStore: writable<{ prompt?: string }>({}),
  };
});

import {
  handleNotificationConsentRequest,
  NOTIFICATION_CONSENT_METHOD,
} from "./notificationConsent";
import { notificationConsentStore } from "$lib/stores/notificationConsent.store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { INTERACTION_REQUIRED_ERROR_CODE } from "$lib/utils/transport/utils";
import { waitForStore } from "$lib/utils/utils";
import { validateDerivationOrigin } from "$lib/utils/validateDerivationOrigin";
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
}: {
  params?: unknown;
  /** Send a request with no `params` member, as an app with nothing to pass does. */
  omitParams?: boolean;
  method?: string;
  settle?: boolean;
} = {}) => {
  const sent: Record<string, unknown>[] = [];
  const errors: string[] = [];
  const channel = {
    origin: ORIGIN,
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

  /** The canister refuses every notification endpoint while its own install
   *  argument is off, so a ceremony offered here could only fail. */
  it("ignores the request while the feature is off", async () => {
    (PUSH_NOTIFICATIONS as unknown as Writable<boolean>).set(false);
    const { sent, errors } = await run({ settle: false });
    expect(sent).toEqual([]);
    expect(errors).toEqual([]);
  });

  it("reports what the canister recorded", async () => {
    const { sent } = await run();
    expect(sent).toHaveLength(1);
    expect(sent[0].result).toEqual({ granted: true });
    expect(consentStatus).toHaveBeenCalledWith({
      anchor_number: BigInt(10_000),
      origin: ORIGIN,
    });
  });

  it("accepts a request that carries no params", async () => {
    const { sent, errors } = await run({ omitParams: true });
    expect(sent[0].result).toEqual({ granted: true });
    expect(errors).toEqual([]);
  });

  it("reports a refusal rather than failing", async () => {
    consentStatus.mockResolvedValue(false);
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
    expect(setRequestContext).not.toHaveBeenCalled();
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
