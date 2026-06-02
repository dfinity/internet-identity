import { describe, it, expect } from "vitest";

// openID imports transitively load featureFlags (localStorage) and globals
// (canister config). Mock the canister-config globals before the import.
import { vi } from "vitest";
vi.mock("$lib/globals", () => ({
  backendCanisterConfig: { openid_configs: [] },
  canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
  frontendCanisterConfig: { dummy_auth: [] },
  anonymousActor: {},
}));
vi.mock("$lib/state/featureFlags", () => ({
  GUIDED_UPGRADE: { subscribe: vi.fn() },
  MIN_GUIDED_UPGRADE: { subscribe: vi.fn() },
  DUMMY_AUTH: { subscribe: vi.fn() },
}));

import { isOpenIdCancelError } from "$lib/utils/openID";
import { CallbackPopupClosedError } from "../../../../routes/(new-styling)/callback/utils";

describe("isOpenIdCancelError — cancel detection for OIDC toast", () => {
  it("returns true for CallbackPopupClosedError (popup closed by user)", () => {
    expect(isOpenIdCancelError(new CallbackPopupClosedError())).toBe(true);
  });

  it("returns true for a NetworkError (FedCM cancel)", () => {
    const networkError = Object.assign(new Error("failed"), {
      name: "NetworkError",
    });
    expect(isOpenIdCancelError(networkError)).toBe(true);
  });

  it("returns false for a generic Error", () => {
    expect(isOpenIdCancelError(new Error("something bad"))).toBe(false);
  });

  it("returns false for a TypeError", () => {
    expect(isOpenIdCancelError(new TypeError("bad type"))).toBe(false);
  });

  it("returns false for a plain string", () => {
    expect(isOpenIdCancelError("cancel")).toBe(false);
  });

  it("returns false for null", () => {
    expect(isOpenIdCancelError(null)).toBe(false);
  });
});
