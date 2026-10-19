import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  CallbackPopupClosedError,
  createRedirectURL,
  FedCmCancelError,
  findConfig,
  issuerMatches,
  extractIssuerTemplateClaims,
  selectAuthScopes,
  extractIdTokenFromCallback,
  isOpenIdCancelError,
  OAuthProviderError,
  requestWithPopup,
  type RequestConfig,
} from "./openID";
import { OpenIdConfig } from "$lib/generated/internet_identity_types";
import { backendCanisterConfig } from "$lib/globals";
import {
  OPENID_AUTHORIZE_STATE_KEY,
  OPENID_RETURN_PATH_KEY,
  OPENID_WIZARD_RESUME_KEY,
} from "$lib/utils/openIdRedirectMarkers";

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

  it("falls back to template-shape regex when a claim is missing", () => {
    // Legacy LastUsedIdentity entries (and cross-env localStorage) can
    // reach issuerMatches without metadata. Match by template shape so the
    // provider's name + logo still resolve on the display path.
    const tid = "4a435c5e-6451-4c1a-a81f-ab9666b6de8f";
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        `https://login.microsoftonline.com/${tid}/v2.0`,
        [],
      ),
    ).toBe(true);
  });

  it("regex fallback rejects an issuer whose shape differs from the template", () => {
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/v2.0",
        [],
      ),
    ).toBe(false);
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://accounts.google.com/abc/v2.0",
        [],
      ),
    ).toBe(false);
  });

  it("regex fallback treats a placeholder as a single path segment", () => {
    // `{tid}` -> `[^/]+`, so a value containing a `/` doesn't sneak past
    // the structural check.
    expect(
      issuerMatches(
        "https://login.microsoftonline.com/{tid}/v2.0",
        "https://login.microsoftonline.com/abc/def/v2.0",
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

  it("matches template issuer via shape regex when required claim is missing", () => {
    // Display-path callers (e.g. SwitchAccessMethod resolving the previous
    // method's name + logo) often pass empty metadata for credentials whose
    // last-used record predates metadata-on-store. The fallback regex on
    // issuerMatches keeps the provider lookup working in that case.
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
    ).toBe(msCfg);
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
    // round-trip, so any shape can arrive — including a bare URL string
    // rather than the expected object.
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

  it("separates the scopes with a percent-encoded space", () => {
    // Apple wants `scope=openid%20email`, not `scope=openid+email`.
    const url = createRedirectURL(
      {
        clientId: "test-client",
        authURL: "https://appleid.apple.com/auth/authorize",
        authScope: "openid email",
      },
      { nonce: "test-nonce", loginHint: "first+tag@example.com" },
    );
    expect(url.href).toContain("scope=openid%20email");
    expect(url.href).not.toContain("+");
    expect(url.searchParams.get("scope")).toBe("openid email");
    expect(url.searchParams.get("login_hint")).toBe("first+tag@example.com");
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
    expect(isOpenIdCancelError(new CallbackPopupClosedError("closed"))).toBe(
      true,
    );
  });

  it("returns true for CallbackPopupClosedError (popup blocked)", () => {
    expect(isOpenIdCancelError(new CallbackPopupClosedError("blocked"))).toBe(
      true,
    );
  });

  it("returns true for a FedCM cancel", () => {
    const networkError = Object.assign(new Error("failed"), {
      name: "NetworkError",
    });
    expect(isOpenIdCancelError(new FedCmCancelError(networkError))).toBe(true);
  });

  it("returns false for a bare NetworkError that did not come from FedCM", () => {
    // A `NetworkError` from any later step (the canister call, the anchor
    // lookup) is a real failure, not a cancelled interaction.
    const networkError = Object.assign(new Error("failed"), {
      name: "NetworkError",
    });
    expect(isOpenIdCancelError(networkError)).toBe(false);
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

describe("requestWithPopup", () => {
  /**
   * Stand-in for the opener's end of the `BroadcastChannel`, so the test
   * decides exactly when the callback page's message lands relative to the
   * close poll. Like a real channel, it drops messages once closed.
   */
  class FakeBroadcastChannel {
    static instances: FakeBroadcastChannel[] = [];
    readonly #listeners: ((event: MessageEvent) => void)[] = [];
    closed = false;

    constructor(readonly name: string) {
      FakeBroadcastChannel.instances.push(this);
    }

    addEventListener(
      _type: "message",
      listener: (event: MessageEvent) => void,
    ) {
      this.#listeners.push(listener);
    }

    close() {
      this.closed = true;
    }

    deliver(data: unknown) {
      if (this.closed) {
        return;
      }
      this.#listeners.forEach((listener) =>
        listener(new MessageEvent("message", { data })),
      );
    }
  }

  const config: RequestConfig = {
    clientId: "client",
    authURL: "https://idp.example/authorize",
    authScope: "openid",
  };

  const openPopup = () => {
    const popup = { closed: false, close: vi.fn(), location: { href: "" } };
    const open = vi
      .spyOn(window, "open")
      .mockReturnValue(popup as unknown as Window);
    const result = requestWithPopup(config, { nonce: "nonce" });
    const state = new URL(open.mock.calls[0][0] as string).searchParams.get(
      "state",
    );
    const [channel] = FakeBroadcastChannel.instances;
    return { popup, result, state, channel };
  };

  beforeEach(() => {
    vi.useFakeTimers();
    FakeBroadcastChannel.instances = [];
    vi.stubGlobal("BroadcastChannel", FakeBroadcastChannel);
  });

  afterEach(() => {
    vi.useRealTimers();
    vi.unstubAllGlobals();
    vi.restoreAllMocks();
  });

  it("opens a deferred popup to /sso-connecting synchronously, then navigates the same window", async () => {
    const popup = { closed: false, close: vi.fn(), location: { href: "" } };
    const open = vi
      .spyOn(window, "open")
      .mockReturnValue(popup as unknown as Window);
    let resolveConfig: (config: RequestConfig) => void = () => {};
    const deferredConfig = new Promise<RequestConfig>((resolve) => {
      resolveConfig = resolve;
    });

    const result = requestWithPopup(deferredConfig, { nonce: "nonce" });

    expect(open).toHaveBeenCalledWith(
      "/sso-connecting",
      "_blank",
      expect.any(String),
    );
    expect(popup.location.href).toBe("");

    resolveConfig(config);
    await vi.waitFor(() =>
      expect(popup.location.href).toMatch(
        /^https:\/\/idp\.example\/authorize\?/,
      ),
    );
    expect(open).toHaveBeenCalledTimes(1);

    popup.closed = true;
    const rejection = expect(result).rejects.toBeInstanceOf(
      CallbackPopupClosedError,
    );
    await vi.advanceTimersByTimeAsync(2000);
    await rejection;
  });

  it("resolves with the token when the callback's message lands after the popup reads as closed", async () => {
    const { popup, result, state, channel } = openPopup();

    // The callback page posts the payload and closes itself back to back.
    // The popup already reads as closed while the message is in flight.
    popup.closed = true;
    // Polls at 250, 500, 750 and 1000 ms all see the popup closed, 750 ms
    // into the grace period that starts at the first of them.
    await vi.advanceTimersByTimeAsync(1000);
    expect(channel.closed).toBe(false);

    channel.deliver({ id_token: "token", state });

    await expect(result).resolves.toBe("token");
    expect(channel.closed).toBe(true);
    expect(popup.close).toHaveBeenCalled();
  });

  it("rejects with CallbackPopupClosedError once the grace period passes without a message", async () => {
    const { popup, result, channel } = openPopup();
    let error: unknown;
    result.catch((e: unknown) => {
      error = e;
    });

    popup.closed = true;
    // The first poll to see the popup closed is at 250 ms, so the 1 s grace
    // period runs out at the 1250 ms poll.
    await vi.advanceTimersByTimeAsync(1000);
    expect(error).toBeUndefined();
    expect(channel.closed).toBe(false);

    await vi.advanceTimersByTimeAsync(250);
    expect(error).toBeInstanceOf(CallbackPopupClosedError);
    expect((error as CallbackPopupClosedError).reason).toBe("closed");
    expect(channel.closed).toBe(true);
  });

  it("listens on the callback channel before the popup is opened", () => {
    // Safari can run the whole Apple ID authorization inside `window.open`
    // (system dialog) and have the landing page post the payload before
    // `window.open` returns, so the listener must already exist by then.
    let channelsWhenOpened: number | undefined;
    const popup = { closed: false, close: vi.fn(), location: { href: "" } };
    vi.spyOn(window, "open").mockImplementation(() => {
      channelsWhenOpened = FakeBroadcastChannel.instances.length;
      return popup as unknown as Window;
    });

    void requestWithPopup(config, { nonce: "nonce" }).catch(() => {});

    expect(channelsWhenOpened).toBe(1);
  });

  it("resolves with the token when window.open returns null but the payload still arrives within the grace period", async () => {
    const open = vi.spyOn(window, "open").mockReturnValue(null);
    const result = requestWithPopup(config, { nonce: "nonce" });
    const state = new URL(open.mock.calls[0][0] as string).searchParams.get(
      "state",
    );
    const [channel] = FakeBroadcastChannel.instances;

    await vi.advanceTimersByTimeAsync(750);
    expect(channel.closed).toBe(false);
    channel.deliver({ id_token: "token", state });

    await expect(result).resolves.toBe("token");
    expect(channel.closed).toBe(true);
  });

  it("rejects as blocked once the grace period passes after window.open returned null", async () => {
    vi.spyOn(window, "open").mockReturnValue(null);
    const result = requestWithPopup(config, { nonce: "nonce" });
    let error: unknown;
    result.catch((e: unknown) => {
      error = e;
    });
    const [channel] = FakeBroadcastChannel.instances;

    // The grace period starts at the `null` return; the poll at 1000 ms is
    // the first one past it.
    await vi.advanceTimersByTimeAsync(750);
    expect(error).toBeUndefined();
    expect(channel.closed).toBe(false);

    await vi.advanceTimersByTimeAsync(500);
    expect(error).toBeInstanceOf(CallbackPopupClosedError);
    expect((error as CallbackPopupClosedError).reason).toBe("blocked");
    expect(channel.closed).toBe(true);
  });

  it("does not navigate a deferred popup that was blocked, and still rejects as blocked", async () => {
    vi.spyOn(window, "open").mockReturnValue(null);
    const result = requestWithPopup(Promise.resolve(config), {
      nonce: "nonce",
    });
    const rejection = expect(result).rejects.toMatchObject({
      reason: "blocked",
    });
    await vi.advanceTimersByTimeAsync(2000);
    await rejection;
  });

  it("accepts the payload posted to the opener window from the same origin", async () => {
    const { popup, result, state, channel } = openPopup();

    window.dispatchEvent(
      new MessageEvent("message", {
        data: { id_token: "token", state },
        origin: window.location.origin,
      }),
    );

    await expect(result).resolves.toBe("token");
    expect(channel.closed).toBe(true);
    expect(popup.close).toHaveBeenCalled();
  });

  it("ignores a payload posted to the opener window from another origin", async () => {
    const { popup, result, state } = openPopup();
    let error: unknown;
    result.catch((e: unknown) => {
      error = e;
    });

    window.dispatchEvent(
      new MessageEvent("message", {
        data: { id_token: "token", state },
        origin: "https://evil.example",
      }),
    );
    await vi.advanceTimersByTimeAsync(0);
    expect(error).toBeUndefined();
    expect(popup.close).not.toHaveBeenCalled();

    // Still waiting on the real payload: closing the popup ends the flow.
    popup.closed = true;
    await vi.advanceTimersByTimeAsync(2000);
    expect(error).toBeInstanceOf(CallbackPopupClosedError);
  });

  it("clears stale same-tab markers before opening the popup, so the popup's landing page posts back instead of navigating", () => {
    sessionStorage.setItem(OPENID_RETURN_PATH_KEY, "/");
    sessionStorage.setItem(OPENID_AUTHORIZE_STATE_KEY, "stale");
    sessionStorage.setItem(OPENID_WIZARD_RESUME_KEY, "{}");
    let markersWhenOpened: (string | null)[] | undefined;
    const popup = { closed: false, close: vi.fn(), location: { href: "" } };
    vi.spyOn(window, "open").mockImplementation(() => {
      markersWhenOpened = [
        sessionStorage.getItem(OPENID_RETURN_PATH_KEY),
        sessionStorage.getItem(OPENID_AUTHORIZE_STATE_KEY),
        sessionStorage.getItem(OPENID_WIZARD_RESUME_KEY),
      ];
      return popup as unknown as Window;
    });

    void requestWithPopup(config, { nonce: "nonce" }).catch(() => {});

    expect(markersWhenOpened).toEqual([null, null, null]);
  });
});
