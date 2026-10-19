import { describe, it, expect, vi, beforeEach } from "vitest";
import { OPENID_PENDING_REQUESTS_KEY } from "$lib/utils/openIdRedirectMarkers";
import type { JsonRequest } from "$lib/utils/transport/utils";

// The module pulls in the channel/session stores and the analytics funnel at
// import time; stub them so the test can exercise the stash/consume logic alone.
vi.mock("$lib/stores/channelStore", () => ({
  establishedChannelStore: { subscribe: () => () => {} },
}));
vi.mock("$lib/stores/session.store", () => ({
  sessionStore: {
    subscribe: (run: (value: { nonce: string }) => void) => {
      run({ nonce: "test-nonce" });
      return () => {};
    },
  },
}));
vi.mock("$lib/utils/analytics/DirectOpenIdFunnel", () => ({
  DirectOpenIdEvents: {},
  directOpenIdFunnel: { trigger: () => {} },
}));

import { consumePendingAuthorizeRequests } from "./openIdRedirect";

const delegationRequest: JsonRequest = {
  jsonrpc: "2.0",
  id: "abc",
  method: "icrc34_delegation",
  params: { publicKey: "AAAA" },
};

describe("consumePendingAuthorizeRequests", () => {
  beforeEach(() => {
    sessionStorage.clear();
  });

  it("returns an empty list when nothing was stashed", () => {
    expect(consumePendingAuthorizeRequests()).toEqual([]);
  });

  it("round-trips the stashed requests", () => {
    sessionStorage.setItem(
      OPENID_PENDING_REQUESTS_KEY,
      JSON.stringify([delegationRequest]),
    );
    expect(consumePendingAuthorizeRequests()).toEqual([delegationRequest]);
  });

  it("is single-use: a second read finds nothing", () => {
    sessionStorage.setItem(
      OPENID_PENDING_REQUESTS_KEY,
      JSON.stringify([delegationRequest]),
    );
    consumePendingAuthorizeRequests();
    expect(consumePendingAuthorizeRequests()).toEqual([]);
    expect(sessionStorage.getItem(OPENID_PENDING_REQUESTS_KEY)).toBeNull();
  });

  it("ignores malformed JSON", () => {
    sessionStorage.setItem(OPENID_PENDING_REQUESTS_KEY, "{not json");
    expect(consumePendingAuthorizeRequests()).toEqual([]);
  });

  it("ignores a payload that isn't an array of JSON-RPC requests", () => {
    sessionStorage.setItem(
      OPENID_PENDING_REQUESTS_KEY,
      JSON.stringify({ not: "an array" }),
    );
    expect(consumePendingAuthorizeRequests()).toEqual([]);
    sessionStorage.setItem(
      OPENID_PENDING_REQUESTS_KEY,
      JSON.stringify([{ missing: "jsonrpc" }]),
    );
    expect(consumePendingAuthorizeRequests()).toEqual([]);
  });
});
