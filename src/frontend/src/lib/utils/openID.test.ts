import { describe, it, expect, vi } from "vitest";
import { findConfig, GOOGLE_ISSUER, issuerMatches } from "./openID";
import {
  GoogleOpenIdConfig,
  OpenIdConfig,
} from "$lib/generated/internet_identity_types";
import { canisterConfig } from "$lib/globals";
import { ENABLE_GENERIC_OPEN_ID } from "$lib/state/featureFlags";

vi.mock("$lib/globals", () => ({
  canisterConfig: {
    openid_configs: [],
  },
}));

describe("issuerMatches", () => {
  it("returns true for exact match without placeholders", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.google.com",
      ),
    ).toBe(true);
  });

  it("returns false for non-matching exact strings", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.example.com",
      ),
    ).toBe(false);
  });

  it("matches a single placeholder as one path segment", () => {
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/4a435c5e-6451-4c1a-a81f-ab9666b6de8f/v2.0",
      ),
    ).toBe(true);
  });

  it("does not allow placeholder to match across path segments", () => {
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/foo/bar/v2.0",
      ),
    ).toBe(false);
  });

  it("matches multiple placeholders each to a single segment", () => {
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/end",
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/extra/end",
      ),
    ).toBe(false);
  });

  it("treats unmatched braces literally (no regex)", () => {
    // Only opening brace, no closing brace -> literal comparison
    const pattern = "https://example.com/{tid";
    expect(issuerMatches(pattern, pattern)).toBe(true);
    expect(issuerMatches(pattern, "https://example.com/anything")).toBe(false);
  });

  it("escapes regex special characters in literals", () => {
    expect(
      issuerMatches("https://example.com/v2.0", "https://example.com/v2.0"),
    ).toBe(true);
    expect(
      issuerMatches("https://example.com/v2.0", "https://example.com/v20"),
    ).toBe(false);

    // Placeholder next to a dot
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v123.0",
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v/123.0",
      ),
    ).toBe(false);
  });
});

const createOpenIDConfig = (issuer: string): OpenIdConfig => ({
  auth_uri: "test",
  jwks_uri: "test",
  logo: "test",
  name: "test",
  fedcm_uri: [],
  issuer,
  auth_scope: ["test"],
  client_id: "test",
});

describe("findConfig", () => {
  const appleIssuer = "https://appleid.apple.com";
  const googleCfg: GoogleOpenIdConfig = { client_id: "gid-123" };

  beforeEach(() => {
    canisterConfig.openid_configs = [];
    canisterConfig.openid_google = [];
  });

  describe("when generic OpenID is disabled", () => {
    beforeEach(() => {
      ENABLE_GENERIC_OPEN_ID.set(false);
    });

    it("returns Google config if issuer is Google", () => {
      canisterConfig.openid_google = [[googleCfg]];
      expect(findConfig(GOOGLE_ISSUER)).toBe(googleCfg);
    });

    it("returns undefined if issuer is Apple", () => {
      canisterConfig.openid_google = [[googleCfg]];
      canisterConfig.openid_configs = [[createOpenIDConfig(appleIssuer)]];
      expect(findConfig(appleIssuer)).toBeUndefined();
    });

    it("returns undefined if issuer is Google but there is no google config", () => {
      canisterConfig.openid_google = [];
      expect(findConfig(GOOGLE_ISSUER)).toBeUndefined();
    });
  });

  describe("when generic OpenID is enabled", () => {
    beforeEach(() => {
      ENABLE_GENERIC_OPEN_ID.set(true);
    });

    it("finds a non-template issuer in openid_configs", () => {
      const cfg = createOpenIDConfig("https://example.com/oauth2");
      canisterConfig.openid_configs = [[cfg]];
      expect(findConfig("https://example.com/oauth2")).toBe(cfg);
    });

    it("returns Apple config if issuer is Apple", () => {
      canisterConfig.openid_google = [[googleCfg]];
      const appleConfig = createOpenIDConfig(appleIssuer);
      canisterConfig.openid_configs = [[appleConfig]];
      expect(findConfig(appleIssuer)).toBe(appleConfig);
    });

    it("returns undefined for google if no open id configs but google config is set", () => {
      canisterConfig.openid_google = [[googleCfg]];
      canisterConfig.openid_configs = [];
      expect(findConfig(GOOGLE_ISSUER)).toBeUndefined();
    });

    it("matches a template issuer in openid_configs", () => {
      const msCfg = createOpenIDConfig(
        "https://login.microsoftonline.com/{tid}/v2.0",
      );
      canisterConfig.openid_configs = [[msCfg]];
      expect(
        findConfig(
          "https://login.microsoftonline.com/4a435c5e-6451-4c1a-a81f-ab9666b6de8f/v2.0",
        ),
      ).toBe(msCfg);
    });

    it("returns undefined when no issuer matches", () => {
      const cfgs = [
        createOpenIDConfig("https://example.com/oauth2"),
        createOpenIDConfig("https://login.microsoftonline.com/{tid}/v2.0"),
      ];
      canisterConfig.openid_configs = [cfgs];
      expect(findConfig("https://no-such-issuer.example.com")).toBeUndefined();
    });
  });
});
