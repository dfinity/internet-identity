import { describe, it, expect, vi } from "vitest";

// openID imports transitively load featureFlags (localStorage) and globals
// (canister config). Mock the canister-config globals before the import.
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
import {
  resolveOpenIdAlreadyLinkedDispatcher,
  resolveOpenIdNotConnectedDispatcher,
} from "./AuthWizard.gating";

describe("AuthWizard gating — OpenID disambiguation dispatcher resolution", () => {
  const handler = () => undefined;

  describe("alreadyLinked", () => {
    it("dispatches in signup mode when result is signIn", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signup", handler),
      ).toBe(handler);
    });

    it("does NOT dispatch in both mode (only signup gates the dialog)", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "both", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch in signin mode", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signin", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch when the handler is absent", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signIn", "signup", undefined),
      ).toBeUndefined();
    });

    it("does NOT dispatch when result is signUp", () => {
      expect(
        resolveOpenIdAlreadyLinkedDispatcher("signUp", "signup", handler),
      ).toBeUndefined();
    });
  });

  describe("notConnected", () => {
    it("dispatches in signin mode when result is signUp", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signin", handler),
      ).toBe(handler);
    });

    it("does NOT dispatch in both mode (only signin gates the dialog)", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "both", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch in signup mode", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signup", handler),
      ).toBeUndefined();
    });

    it("does NOT dispatch when the handler is absent", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signUp", "signin", undefined),
      ).toBeUndefined();
    });

    it("does NOT dispatch when result is signIn", () => {
      expect(
        resolveOpenIdNotConnectedDispatcher("signIn", "signin", handler),
      ).toBeUndefined();
    });
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
