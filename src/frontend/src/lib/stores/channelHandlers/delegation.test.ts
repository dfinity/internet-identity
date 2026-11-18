import { afterEach, describe, expect, it, vi } from "vitest";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
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
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin: vi.fn(() => Promise.resolve({ result: "valid" })),
}));
vi.mock("$lib/stores/authentication.store", async () => {
  const { writable } = await import("svelte/store");
  return { authenticationStore: writable<unknown>(undefined) };
});
vi.mock("$lib/stores/attributeConsent.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    attributeConsentStore: writable<unknown>(undefined),
    attributeConsentResultStore: writable<unknown>(undefined),
  };
});
vi.mock("$lib/stores/authorization.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    authorizationStore: { setRequestContext: () => {} },
    authorizedStore: writable<unknown>(undefined),
  };
});

/** What the notification check answers, call by call, and what it does as it answers. */
const notificationCheck = vi.hoisted(() => {
  const state: {
    outcomes: ("done" | "identity-changed")[];
    onIdentityChanged: () => void;
  } = { outcomes: [], onIdentityChanged: () => {} };
  return state;
});
vi.mock("$lib/stores/channelHandlers/notificationRepair", () => ({
  repairNotificationPath: () => {
    const outcome = notificationCheck.outcomes.shift() ?? "done";
    if (outcome === "identity-changed") {
      notificationCheck.onIdentityChanged();
    }
    return Promise.resolve(outcome);
  },
}));

import { handleDelegationRequest } from "./delegation";
import { authenticationStore } from "$lib/stores/authentication.store";
import { authorizedStore } from "$lib/stores/authorization.store";
import { authorizeAddedIdentity } from "../../../routes/(new-styling)/authorize/addIdentity";

// Both are mocked as plain writables above; only their real types are in scope here.
const signedIn = authenticationStore as unknown as Writable<unknown>;
const authorized = authorizedStore as unknown as Writable<unknown>;

const authorizedNow = () =>
  authorized.set({
    accountNumberPromise: Promise.resolve(undefined),
    accessLevel: "full-access",
    maxTimeToLive: undefined,
  });

/** Runs one delegation request to the end, answering for the canister with a chain the
 *  app's key could use. Answers what the app was sent and how often it was minted for.
 *  `firstLifetimeMs` is how long the first credentials minted are valid for. */
const runRequest = async (firstLifetimeMs = 60 * 60 * 1000) => {
  const appKey = await ECDSAKeyIdentity.generate({ extractable: true });
  const expiration = BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);
  const chain = await DelegationChain.create(
    appKey,
    appKey.getPublicKey(),
    new Date(Number(expiration / BigInt(1_000_000))),
  );
  const signed = chain.delegations[0];
  let numMinted = 0;
  const actor = {
    prepare_account_delegation: () => {
      numMinted += 1;
      const lifetimeMs = numMinted === 1 ? firstLifetimeMs : 60 * 60 * 1000;
      return Promise.resolve({
        Ok: {
          user_key: new Uint8Array(chain.publicKey),
          expiration: BigInt(Date.now() + lifetimeMs) * BigInt(1_000_000),
        },
      });
    },
    get_account_delegation: () =>
      Promise.resolve({
        Ok: {
          delegation: {
            pubkey: new Uint8Array(signed.delegation.pubkey),
            expiration: signed.delegation.expiration,
            targets: [],
            permissions: [],
          },
          signature: new Uint8Array(signed.signature),
        },
      }),
  };
  signedIn.set({
    identityNumber: BigInt(10_000),
    actor,
    authMethod: { passkey: { credentialId: new Uint8Array() } },
  });
  authorizedNow();

  const sent: unknown[] = [];
  const channel = {
    origin: ORIGIN,
    closed: false,
    resumeToken: "token",
    addEventListener: () => () => {},
    send: (response: unknown) => {
      sent.push(response);
      return Promise.resolve();
    },
    close: async () => {},
  };
  const der = new Uint8Array(appKey.getPublicKey().toDer());
  await handleDelegationRequest(
    channel,
    vi.fn(),
  )({
    jsonrpc: "2.0",
    id: 1,
    method: "icrc34_delegation",
    params: { publicKey: btoa(String.fromCharCode(...der)) },
  });
  return { sent, numMinted };
};

describe("an identity change while notifications are checked", () => {
  afterEach(() => {
    notificationCheck.outcomes = [];
    notificationCheck.onIdentityChanged = () => {};
  });

  it("answers the app with a delegation minted for the identity the user authorizes next", async () => {
    notificationCheck.outcomes = ["identity-changed", "done"];
    // The user switches and authorizes again while the check runs.
    notificationCheck.onIdentityChanged = authorizedNow;

    const { sent, numMinted } = await runRequest();

    expect(numMinted).toBe(2);
    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1, result: {} });
  });

  it("answers the app for an identity added from the header", async () => {
    notificationCheck.outcomes = ["identity-changed", "done"];
    // The user adds an identity from the header while the check runs, which authorizes
    // it as the layout's Add identity does.
    notificationCheck.onIdentityChanged = () =>
      void authorizeAddedIdentity(() => {
        authorizedNow();
        return Promise.resolve();
      });

    const { sent, numMinted } = await runRequest();

    expect(numMinted).toBe(2);
    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1, result: {} });
  });
});

describe("credentials minted before the allow screen", () => {
  afterEach(() => {
    notificationCheck.outcomes = [];
  });

  it("mints again once when the screen left them less than a minute", async () => {
    notificationCheck.outcomes = ["done"];

    const { sent, numMinted } = await runRequest(30_000);

    expect(numMinted).toBe(2);
    expect(sent).toHaveLength(1);
  });

  it("hands over what was minted while it has time left", async () => {
    notificationCheck.outcomes = ["done"];

    const { sent, numMinted } = await runRequest();

    expect(numMinted).toBe(1);
    expect(sent).toHaveLength(1);
  });
});
