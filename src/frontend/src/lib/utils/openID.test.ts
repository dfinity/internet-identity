import { describe, it, expect, vi } from "vitest";
import {
  findConfig,
  GOOGLE_ISSUER,
  issuerMatches,
  extractIssuerTemplateClaims,
} from "./openID";
import {
  GoogleOpenIdConfig,
  OpenIdConfig,
} from "$lib/generated/internet_identity_types";
import { canisterConfig } from "$lib/globals";

vi.mock("$lib/globals", () => ({
  canisterConfig: {
    openid_configs: [],
    openid_google: [],
  },
}));

describe("issuerMatches", () => {
  it("returns true for exact match without placeholders", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.google.com",
        [],
      ),
    ).toBe(true);
  });

  it.only("returns false for half match", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.google.com/123/v2.0",
        [],
      ),
    ).toBe(false);
  });

  it("returns false for non-matching exact strings", () => {
    expect(
      issuerMatches(
        "https://accounts.google.com",
        "https://accounts.example.com",
        [],
      ),
    ).toBe(false);
  });

  it("builds issuer by substituting a single placeholder from claims", () => {
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        `https://login.microsoftonline.com/${tid}/v2.0`,
        [["tid", { String: tid }]],
      ),
    ).toBe(true);
  });

  it("returns false if a required placeholder is missing in claims", () => {
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        `https://login.microsoftonline.com/${tid}/v2.0`,
        [],
      ),
    ).toBe(false);
  });

  it("substitutes multiple placeholders from claims", () => {
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/end",
        [
          ["a", { String: "one" }],
          ["b", { String: "two" }],
        ],
      ),
    ).toBe(true);
  });

  it("uses claim values verbatim (including slashes)", () => {
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/extra/end",
        [
          ["a", { String: "one" }],
          ["b", { String: "two/extra" }],
        ],
      ),
    ).toBe(true);
    // If claims include a slash, the exact built string must match
    expect(
      issuerMatches(
        "https://example.com/{a}/{b}/end",
        "https://example.com/one/two/end",
        [
          ["a", { String: "one" }],
          ["b", { String: "two/extra" }],
        ],
      ),
    ).toBe(false);
  });

  it("treats unmatched braces literally (no regex)", () => {
    // Only opening brace, no closing brace -> literal comparison
    const pattern = "https://example.com/{tid";
    expect(issuerMatches(pattern, pattern, [])).toBe(true);
    expect(issuerMatches(pattern, "https://example.com/anything", [])).toBe(
      false,
    );
  });

  it("escapes regex special characters in literals", () => {
    expect(
      issuerMatches("https://example.com/v2.0", "https://example.com/v2.0", []),
    ).toBe(true);
    expect(
      issuerMatches("https://example.com/v2.0", "https://example.com/v20", []),
    ).toBe(false);

    // Placeholder next to a dot
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v123.0",
        [["ver", { String: "123" }]],
      ),
    ).toBe(true);
    expect(
      issuerMatches(
        "https://example.com/v{ver}.0",
        "https://example.com/v/123.0",
        [["ver", { String: "123" }]],
      ),
    ).toBe(false);
  });
});

describe("extractIssuerTemplateClaims", () => {
  it("returns empty array for template without placeholders", () => {
    expect(extractIssuerTemplateClaims("https://example.com")).toEqual([]);
  });

  it("extracts a single placeholder name", () => {
    expect(
      extractIssuerTemplateClaims(
        "https://login.microsoftonline.com/{tid}/v2.0",
      ),
    ).toEqual(["tid"]);
  });

  it("extracts multiple placeholder names preserving order", () => {
    expect(
      extractIssuerTemplateClaims("https://example.com/{a}/{b}/end"),
    ).toEqual(["a", "b"]);
  });

  it("ignores unmatched braces and only captures balanced placeholders", () => {
    // Only opening brace, no closing brace -> no placeholders
    expect(extractIssuerTemplateClaims("https://example.com/{tid")).toEqual([]);
    // Balanced placeholders still work
    expect(extractIssuerTemplateClaims("https://ex.com/{x}/v{y}.0")).toEqual([
      "x",
      "y",
    ]);
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

  it("returns OpenID config when issuer matches in openid_configs", () => {
    const cfg = createOpenIDConfig("https://example.com/oauth2");
    canisterConfig.openid_configs = [[cfg]];
    expect(findConfig("https://example.com/oauth2", [])).toBe(cfg);
  });

  it("matches a template issuer in openid_configs when claims provide values", () => {
    const msCfg = createOpenIDConfig(
      "https://login.microsoftonline.com/{tid}/v2.0",
    );
    canisterConfig.openid_configs = [[msCfg]];
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      findConfig(`https://login.microsoftonline.com/${tid}/v2.0`, [
        ["tid", { String: tid }],
      ]),
    ).toBe(msCfg);
  });

  it("does not match template issuer if required claim is missing", () => {
    const msCfg = createOpenIDConfig(
      "https://login.microsoftonline.com/{tid}/v2.0",
    );
    canisterConfig.openid_configs = [[msCfg]];
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      findConfig(`https://login.microsoftonline.com/${tid}/v2.0`, []),
    ).toBeUndefined();
  });

  it("returns Apple config if issuer is Apple (from openid_configs)", () => {
    canisterConfig.openid_google = [[googleCfg]];
    const appleConfig = createOpenIDConfig(appleIssuer);
    canisterConfig.openid_configs = [[appleConfig]];
    expect(findConfig(appleIssuer, [])).toBe(appleConfig);
  });

  it("returns Google config if issuer is Google and no matching openid_configs", () => {
    canisterConfig.openid_google = [[googleCfg]];
    canisterConfig.openid_configs = [];
    expect(findConfig(GOOGLE_ISSUER, [])).toBe(googleCfg);
  });

  it("prefers openid_configs over Google config for Google issuer when both present", () => {
    const googleOpenIdCfg = createOpenIDConfig(GOOGLE_ISSUER);
    canisterConfig.openid_configs = [[googleOpenIdCfg]];
    canisterConfig.openid_google = [[googleCfg]];
    expect(findConfig(GOOGLE_ISSUER, [])).toBe(googleOpenIdCfg);
  });

  it("returns undefined for Google issuer when no Google config and no matching openid_configs", () => {
    canisterConfig.openid_google = [];
    canisterConfig.openid_configs = [];
    expect(findConfig(GOOGLE_ISSUER, [])).toBeUndefined();
  });

  it("returns undefined when no issuer matches", () => {
    const cfgs = [
      createOpenIDConfig("https://example.com/oauth2"),
      createOpenIDConfig("https://login.microsoftonline.com/{tid}/v2.0"),
    ];
    canisterConfig.openid_configs = [cfgs];
    expect(
      findConfig("https://no-such-issuer.example.com", []),
    ).toBeUndefined();
  });
});
