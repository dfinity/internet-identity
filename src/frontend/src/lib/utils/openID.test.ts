import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  CallbackPopupClosedError,
  createRedirectURL,
  findConfig,
  issuerMatches,
  extractIssuerTemplateClaims,
  selectAuthScopes,
  extractIdTokenFromCallback,
  isOpenIdCancelError,
  OAuthProviderError,
} from "./openID";
import { OpenIdConfig } from "$lib/generated/internet_identity_types";
import { backendCanisterConfig } from "$lib/globals";

vi.mock("$lib/globals", () => ({
  backendCanisterConfig: {
    openid_configs: [],
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

  it("returns false for half match", () => {
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
  email_verification: [],
  seed_jwks: [],
});

describe("findConfig", () => {
  const appleIssuer = "https://appleid.apple.com";
  // Every `createOpenIDConfig(...)` returns a config with client_id="test",
  // so passing "test" as `aud` exercises the direct-match path.
  const DIRECT_AUD = "test";

  beforeEach(() => {
    backendCanisterConfig.openid_configs = [];
  });

  it("returns OpenID config when issuer and aud match in openid_configs", () => {
    const cfg = createOpenIDConfig("https://example.com/oauth2");
    backendCanisterConfig.openid_configs = [[cfg]];
    expect(findConfig("https://example.com/oauth2", DIRECT_AUD, [])).toBe(cfg);
  });

  it("matches a template issuer in openid_configs when claims provide values", () => {
    const msCfg = createOpenIDConfig(
      "https://login.microsoftonline.com/{tid}/v2.0",
    );
    backendCanisterConfig.openid_configs = [[msCfg]];
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      findConfig(`https://login.microsoftonline.com/${tid}/v2.0`, DIRECT_AUD, [
        ["tid", { String: tid }],
      ]),
    ).toBe(msCfg);
  });

  it("does not match template issuer if required claim is missing", () => {
    const msCfg = createOpenIDConfig(
      "https://login.microsoftonline.com/{tid}/v2.0",
    );
    backendCanisterConfig.openid_configs = [[msCfg]];
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      findConfig(
        `https://login.microsoftonline.com/${tid}/v2.0`,
        DIRECT_AUD,
        [],
      ),
    ).toBeUndefined();
  });

  it("returns Apple config if issuer is Apple (from openid_configs)", () => {
    const appleConfig = createOpenIDConfig(appleIssuer);
    backendCanisterConfig.openid_configs = [[appleConfig]];
    expect(findConfig(appleIssuer, DIRECT_AUD, [])).toBe(appleConfig);
  });

  it("returns undefined when no issuer matches", () => {
    const cfgs = [
      createOpenIDConfig("https://example.com/oauth2"),
      createOpenIDConfig("https://login.microsoftonline.com/{tid}/v2.0"),
    ];
    backendCanisterConfig.openid_configs = [cfgs];
    expect(
      findConfig("https://no-such-issuer.example.com", DIRECT_AUD, []),
    ).toBeUndefined();
  });

  it("falls back to issuer-only match when aud doesn't match any config", () => {
    // Legacy/migrated credentials can have an `aud` that doesn't line up
    // with the current `openid_configs` entry (e.g. client_id rotation).
    // We prefer returning SOMETHING so the UI can label these direct-
    // provider credentials correctly. SSO credentials are disambiguated
    // earlier via canister-stamped `sso_domain` / `sso_name` metadata —
    // `openIdName` / `openIdLogo` short-circuit on those before ever
    // consulting `findConfig`, so the issuer-only fallback can't
    // mis-attribute an SSO credential to its underlying IdP.
    const googleCfg = createOpenIDConfig("https://accounts.google.com");
    backendCanisterConfig.openid_configs = [[googleCfg]];
    expect(
      findConfig("https://accounts.google.com", "some-other-client-id", []),
    ).toBe(googleCfg);
  });

  it("prefers the strict (iss, aud) match over the issuer-only fallback", () => {
    // If two configs share an issuer, the one whose client_id matches aud
    // wins over the issuer-only fallback.
    const cfgA = createOpenIDConfig("https://same-issuer.example");
    const cfgB: OpenIdConfig = {
      ...createOpenIDConfig("https://same-issuer.example"),
      client_id: "other-aud",
    };
    backendCanisterConfig.openid_configs = [[cfgA, cfgB]];
    expect(findConfig("https://same-issuer.example", "other-aud", [])).toBe(
      cfgB,
    );
    expect(findConfig("https://same-issuer.example", DIRECT_AUD, [])).toBe(
      cfgA,
    );
  });

  it("falls back to issuer-only matching when aud is undefined", () => {
    // Legacy LastUsedIdentity entries don't track aud; we preserve the old
    // behavior (issuer-only match) for those callers.
    const cfg = createOpenIDConfig("https://example.com/oauth2");
    backendCanisterConfig.openid_configs = [[cfg]];
    expect(findConfig("https://example.com/oauth2", undefined, [])).toBe(cfg);
  });
});

describe("selectAuthScopes", () => {
  it("returns openid + profile + email when scopes_supported is undefined", () => {
    // `openid` is required by the OIDC spec, so we always ask for it.
    expect(selectAuthScopes(undefined)).toEqual(["openid", "profile", "email"]);
  });

  it("always includes openid even if the provider omits it from scopes_supported", () => {
    // Some providers don't advertise `openid` in their /.well-known/openid-
    // configuration scopes_supported list; the spec still requires it.
    expect(selectAuthScopes(["email", "offline_access"])).toEqual([
      "openid",
      "email",
    ]);
  });

  it("keeps optional scopes only if advertised", () => {
    expect(
      selectAuthScopes(["openid", "profile", "email", "offline_access"]),
    ).toEqual(["openid", "profile", "email"]);
  });

  it("returns just openid when no optional scopes are advertised", () => {
    expect(selectAuthScopes(["custom_scope_one"])).toEqual(["openid"]);
  });

  it("returns just openid when scopes_supported is empty", () => {
    expect(selectAuthScopes([])).toEqual(["openid"]);
  });
});

describe("extractIdTokenFromCallback", () => {
  const STATE = "expected-state";

  it("returns the id_token when state matches and no error is present", () => {
    expect(
      extractIdTokenFromCallback(
        { id_token: "eyJhbGciOi.test.token", state: STATE },
        STATE,
      ),
    ).toBe("eyJhbGciOi.test.token");
  });

  it("throws OAuthProviderError with error and error_description", () => {
    // Real-world example: Okta app registered as Web App (code-only)
    // refuses our `response_type=id_token code` hybrid request.
    let thrown: unknown;
    try {
      extractIdTokenFromCallback(
        {
          state: STATE,
          error: "unsupported_response_type",
          error_description:
            "The response type is not supported by the authorization server. Configured response types: [code]",
        },
        STATE,
      );
    } catch (e) {
      thrown = e;
    }
    expect(thrown).toBeInstanceOf(OAuthProviderError);
    const err = thrown as OAuthProviderError;
    expect(err.error).toBe("unsupported_response_type");
    expect(err.errorDescription).toBe(
      "The response type is not supported by the authorization server. Configured response types: [code]",
    );
    expect(err.message).toContain("unsupported_response_type");
    expect(err.message).toContain("The response type is not supported");
  });

  it("throws OAuthProviderError with only error when error_description is absent", () => {
    let thrown: unknown;
    try {
      extractIdTokenFromCallback(
        { state: STATE, error: "access_denied" },
        STATE,
      );
    } catch (e) {
      thrown = e;
    }
    expect(thrown).toBeInstanceOf(OAuthProviderError);
    const err = thrown as OAuthProviderError;
    expect(err.error).toBe("access_denied");
    expect(err.errorDescription).toBeUndefined();
  });

  it("normalizes a null error_description to undefined", () => {
    // The canister serializes an absent error_description as JSON null
    // rather than omitting the key, so the parsed payload carries null.
    let thrown: unknown;
    try {
      extractIdTokenFromCallback(
        { state: STATE, error: "access_denied", error_description: null },
        STATE,
      );
    } catch (e) {
      thrown = e;
    }
    expect(thrown).toBeInstanceOf(OAuthProviderError);
    const err = thrown as OAuthProviderError;
    expect(err.error).toBe("access_denied");
    expect(err.errorDescription).toBeUndefined();
  });

  it("checks state before surfacing a provider error", () => {
    // Guards against a forged callback: an attacker who can inject a
    // payload with a legitimate-looking provider error shouldn't be
    // able to influence user-facing messaging without passing the CSRF
    // check first.
    expect(() =>
      extractIdTokenFromCallback(
        { state: "attacker-state", error: "unsupported_response_type" },
        STATE,
      ),
    ).toThrow("Invalid state");
  });

  it("throws 'Invalid state' when state is missing", () => {
    expect(() =>
      extractIdTokenFromCallback({ id_token: "eyJhbGciOi.test.token" }, STATE),
    ).toThrow("Invalid state");
  });

  it("throws 'Invalid state' when the payload is not an object", () => {
    // The payload crosses a BroadcastChannel / sessionStorage JSON
    // round-trip, so any shape can arrive — including the URL string the
    // legacy fragment-era callback page used to post.
    expect(() =>
      extractIdTokenFromCallback(
        `https://example.id.ai/callback#state=${STATE}&id_token=abc`,
        STATE,
      ),
    ).toThrow("Invalid state");
    expect(() => extractIdTokenFromCallback(undefined, STATE)).toThrow(
      "Invalid state",
    );
    expect(() => extractIdTokenFromCallback(null, STATE)).toThrow(
      "Invalid state",
    );
  });

  it("throws 'Invalid state' when state is not a string", () => {
    expect(() =>
      extractIdTokenFromCallback(
        { id_token: "eyJhbGciOi.test.token", state: [STATE] },
        STATE,
      ),
    ).toThrow("Invalid state");
  });

  it("throws 'No token received' when the provider omits both id_token and error", () => {
    // Fallback for a spec-violating provider (e.g. pure auth-code flow
    // with no token in the POST body — we'd see `code` but no
    // `id_token`). The payload will still have state for our CSRF
    // guard to pass.
    expect(() =>
      extractIdTokenFromCallback({ state: STATE, code: "abc123" }, STATE),
    ).toThrow("No token received");
  });

  it("throws 'No token received' when id_token is not a string", () => {
    expect(() =>
      extractIdTokenFromCallback({ state: STATE, id_token: 42 }, STATE),
    ).toThrow("No token received");
  });
});

describe("createRedirectURL", () => {
  it("requests the form_post response mode", () => {
    const url = createRedirectURL(
      {
        clientId: "test-client",
        authURL: "https://idp.example.com/authorize",
        authScope: "openid profile email",
      },
      { nonce: "test-nonce" },
    );
    expect(url.searchParams.get("response_mode")).toBe("form_post");
    expect(url.searchParams.get("response_type")).toBe("code id_token");
    expect(url.searchParams.get("client_id")).toBe("test-client");
    expect(url.searchParams.get("nonce")).toBe("test-nonce");
    expect(url.searchParams.get("state")).not.toBeNull();
    const redirectUri = url.searchParams.get("redirect_uri");
    expect(redirectUri).not.toBeNull();
    expect(new URL(redirectUri ?? "").pathname).toBe("/callback");
  });
});

describe("OAuthProviderError", () => {
  it("includes error and description in the message when both are present", () => {
    const err = new OAuthProviderError(
      "invalid_scope",
      "The requested scope is not allowed.",
    );
    expect(err.error).toBe("invalid_scope");
    expect(err.errorDescription).toBe("The requested scope is not allowed.");
    expect(err.message).toBe(
      "OAuth provider error: invalid_scope: The requested scope is not allowed.",
    );
    expect(err.name).toBe("OAuthProviderError");
  });

  it("omits the description from the message when it's absent", () => {
    const err = new OAuthProviderError("access_denied");
    expect(err.message).toBe("OAuth provider error: access_denied");
    expect(err.errorDescription).toBeUndefined();
  });

  it("is an Error instance (so existing `instanceof Error` branches still catch it)", () => {
    const err = new OAuthProviderError("server_error");
    expect(err).toBeInstanceOf(Error);
  });
});

describe("isOpenIdCancelError", () => {
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
