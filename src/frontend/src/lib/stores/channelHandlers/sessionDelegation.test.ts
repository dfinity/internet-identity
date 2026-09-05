import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";

const ORIGIN = "https://app.example.com";

vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    canisterId: Principal.fromText("rwlgt-iiaaa-aaaaa-aaaaa-cai"),
    backendCanisterConfig: { openid_configs: [] },
    frontendCanisterConfig: { related_origins: [], dev_csp: [] },
  };
});
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin: vi.fn(() => Promise.resolve({ result: "valid" })),
}));
vi.mock("$lib/utils/iiConnection", () => ({
  remapToLegacyDomain: (origin: string) => origin,
}));

const setRequestContext = vi.fn();
vi.mock("$lib/stores/authorization.store", () => ({
  authorizationStore: {
    setRequestContext: (...args: unknown[]) => setRequestContext(...args),
  },
  authorizedStore: { subscribe: () => () => {} },
}));

import {
  asBrowserKeyError,
  handleSessionDelegationRequest,
} from "./sessionDelegation";
import { StaleBrowserKeyError } from "$lib/stores/browser-key.store";
import { CanisterError } from "$lib/utils/utils";
import { purgeAppSessions } from "$lib/stores/app-session.store";

const channelWith = () => {
  const sent: unknown[] = [];
  return {
    channel: {
      origin: ORIGIN,
      closed: false,
      resumeToken: "token",
      addEventListener: () => () => {},
      send: (response: unknown) => {
        sent.push(response);
        return Promise.resolve();
      },
      close: async () => {},
    },
    sent,
  };
};

describe("ii_session_delegation", () => {
  beforeEach(async () => {
    setRequestContext.mockClear();
    await purgeAppSessions(BigInt(10_000));
    await purgeAppSessions(BigInt(10_001));
  });

  it("ignores a request for another method", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "icrc34_delegation",
    });

    expect(sent).toEqual([]);
    expect(onError).not.toHaveBeenCalled();
  });

  it("rejects params that carry no session key", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: {},
    });

    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1, error: { code: -32602 } });
    expect(onError).toHaveBeenCalledWith("invalid-request");
  });
});

describe("asBrowserKeyError", () => {
  it("names a retired browser key so the key store can promote its successor", () => {
    const stale = asBrowserKeyError(
      new CanisterError({ StaleDeviceKey: null }),
    );

    expect(stale).toBeInstanceOf(StaleBrowserKeyError);
  });

  it("leaves every other canister error alone", () => {
    const other = new CanisterError({ NoSuchAccount: null });

    expect(asBrowserKeyError(other)).toBe(other);
  });

  it("leaves a transport failure alone", () => {
    const network = new Error("network");

    expect(asBrowserKeyError(network)).toBe(network);
  });
});
