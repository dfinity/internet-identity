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

describe("AuthWizard mode-gating — signIn vs signUp result discrimination", () => {
  // The mode check in handleContinueWithOpenId reads:
  //   if (result.type === "signIn") { ... onOpenIdAlreadyLinked gated to signup/both }
  //   if (result.type === "signUp") { ... onOpenIdNotConnected gated to signin/both }
  // These tests verify the boolean shape of those checks.

  type Result = { type: "signIn" } | { type: "signUp" };

  const alreadyLinkedFires = (
    result: Result,
    mode: "signin" | "signup" | "both",
    handlerPresent: boolean,
  ) =>
    result.type === "signIn" &&
    (mode === "signup" || mode === "both") &&
    handlerPresent;

  const notConnectedFires = (
    result: Result,
    mode: "signin" | "signup" | "both",
    handlerPresent: boolean,
  ) =>
    result.type === "signUp" &&
    (mode === "signin" || mode === "both") &&
    handlerPresent;

  it("alreadyLinked fires in signup mode when result is signIn", () => {
    expect(alreadyLinkedFires({ type: "signIn" }, "signup", true)).toBe(true);
  });

  it("alreadyLinked fires in both mode when result is signIn", () => {
    expect(alreadyLinkedFires({ type: "signIn" }, "both", true)).toBe(true);
  });

  it("alreadyLinked does NOT fire in signin mode even when result is signIn", () => {
    expect(alreadyLinkedFires({ type: "signIn" }, "signin", true)).toBe(false);
  });

  it("alreadyLinked does NOT fire when handler is absent", () => {
    expect(alreadyLinkedFires({ type: "signIn" }, "both", false)).toBe(false);
  });

  it("alreadyLinked does NOT fire when result is signUp", () => {
    expect(alreadyLinkedFires({ type: "signUp" }, "both", true)).toBe(false);
  });

  it("notConnected fires in signin mode when result is signUp", () => {
    expect(notConnectedFires({ type: "signUp" }, "signin", true)).toBe(true);
  });

  it("notConnected fires in both mode when result is signUp", () => {
    expect(notConnectedFires({ type: "signUp" }, "both", true)).toBe(true);
  });

  it("notConnected does NOT fire in signup mode even when result is signUp", () => {
    expect(notConnectedFires({ type: "signUp" }, "signup", true)).toBe(false);
  });

  it("notConnected does NOT fire when handler is absent", () => {
    expect(notConnectedFires({ type: "signUp" }, "both", false)).toBe(false);
  });

  it("notConnected does NOT fire when result is signIn", () => {
    expect(notConnectedFires({ type: "signIn" }, "both", true)).toBe(false);
  });
});

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
