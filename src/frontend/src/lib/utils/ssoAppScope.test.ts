import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  currentSsoAppScope,
  readAppScopeConsent,
  writeAppScopeConsent,
} from "./ssoAppScope";
import { SSO_APP_SCOPE } from "$lib/state/featureFlags";
import { authorizationStore } from "$lib/stores/authorization.store";

vi.mock("$lib/globals", () => ({
  getConfiguredFeatureFlag: () => undefined,
  getPrimaryOrigin: () => undefined,
}));

const SSO_DOMAIN = "acme.com";
const APP_ORIGIN = "https://try.id.ai";

describe("ssoAppScope", () => {
  beforeEach(() => {
    localStorage.clear();
    SSO_APP_SCOPE.set(true);
    authorizationStore.setEffectiveOrigin(APP_ORIGIN);
    writeAppScopeConsent(SSO_DOMAIN, APP_ORIGIN, true);
  });

  it("returns the app scope when flag, origin and consent are all present", () => {
    expect(currentSsoAppScope(SSO_DOMAIN)).toBe("icp:try.id.ai");
  });

  it("returns undefined when the feature flag is off", () => {
    SSO_APP_SCOPE.set(false);
    expect(currentSsoAppScope(SSO_DOMAIN)).toBeUndefined();
  });

  it("returns undefined without consent for this (domain, app) pair", () => {
    writeAppScopeConsent(SSO_DOMAIN, APP_ORIGIN, false);
    expect(currentSsoAppScope(SSO_DOMAIN)).toBeUndefined();
  });

  it("does not let consent leak to another SSO domain or app", () => {
    expect(currentSsoAppScope("other-org.com")).toBeUndefined();
    authorizationStore.setEffectiveOrigin("https://other-app.com");
    expect(currentSsoAppScope(SSO_DOMAIN)).toBeUndefined();
  });

  it("treats the SSO domain case-insensitively", () => {
    expect(currentSsoAppScope("ACME.com")).toBe("icp:try.id.ai");
  });

  it("round-trips and revokes consent", () => {
    expect(readAppScopeConsent(SSO_DOMAIN, APP_ORIGIN)).toBe(true);
    writeAppScopeConsent(SSO_DOMAIN, APP_ORIGIN, false);
    expect(readAppScopeConsent(SSO_DOMAIN, APP_ORIGIN)).toBe(false);
    expect(
      localStorage.getItem(`ii:sso-app-scope:${SSO_DOMAIN}|${APP_ORIGIN}`),
    ).toBeNull();
  });
});
