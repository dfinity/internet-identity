import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { get } from "svelte/store";
import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
import { backendCanisterConfig } from "$lib/globals";
import {
  APPLE_SAME_TAB_REDIRECT,
  OPENID_SAME_TAB_REDIRECT,
} from "$lib/state/featureFlags";
import {
  OPENID_AUTHORIZE_STATE_KEY,
  OPENID_CALLBACK_DATA_KEY,
  OPENID_RETURN_PATH_KEY,
  OPENID_WIZARD_RESUME_KEY,
} from "$lib/utils/openIdRedirectMarkers";
import { OAuthProviderError } from "$lib/utils/openID";
import {
  consumeOpenIdSameTabResume,
  hasPendingOpenIdSameTabResume,
  isWebKitBrowser,
  redirectToOpenIdInSameTab,
  shouldRedirectInSameTab,
} from "./openIdSameTabRedirect";

vi.mock("$lib/globals", () => ({
  backendCanisterConfig: {
    openid_configs: [[]],
  },
  getConfiguredFeatureFlag: () => undefined,
  getPrimaryOrigin: () => undefined,
  notificationsEnabled: () => false,
}));

vi.mock("$lib/stores/session.store", () => ({
  sessionStore: {
    subscribe: (run: (value: { nonce: string }) => void) => {
      run({ nonce: "test-nonce" });
      return () => {};
    },
  },
}));

const APPLE_AUTH_URL = "https://appleid.apple.com/auth/authorize";
const GOOGLE_AUTH_URL = "https://accounts.google.com/o/oauth2/v2/auth";

const UA = {
  iphoneSafari:
    "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1",
  iphoneChrome:
    "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) CriOS/129.0.0.0 Mobile/15E148 Safari/604.1",
  ipadDesktopMode:
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
  macSafari:
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
  macChrome:
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
  macEdge:
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36 Edg/129.0.0.0",
  macFirefox:
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 14.6; rv:130.0) Gecko/20100101 Firefox/130.0",
  androidChrome:
    "Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Mobile Safari/537.36",
  androidSamsung:
    "Mozilla/5.0 (Linux; Android 14; SAMSUNG SM-S918B) AppleWebKit/537.36 (KHTML, like Gecko) SamsungBrowser/26.0 Chrome/122.0.0.0 Mobile Safari/537.36",
  windowsChrome:
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
};

const appleConfig: OpenIdConfig = {
  issuer: "https://appleid.apple.com",
  client_id: "ai.id.auth",
  auth_uri: APPLE_AUTH_URL,
  jwks_uri: "https://appleid.apple.com/auth/keys",
  logo: "",
  name: "Apple",
  fedcm_uri: [],
  email_verification: [],
  auth_scope: ["openid", "email", "name"],
  seed_jwks: [],
};

describe("isWebKitBrowser", () => {
  it.each([
    ["iPhone Safari", UA.iphoneSafari, 5, true],
    ["Chrome on iPhone (WebKit underneath)", UA.iphoneChrome, 5, true],
    ["iPad in desktop mode (Mac agent, touch)", UA.ipadDesktopMode, 5, true],
    ["Safari on macOS", UA.macSafari, 0, true],
    ["Chrome on macOS", UA.macChrome, 0, false],
    ["Edge on macOS", UA.macEdge, 0, false],
    ["Firefox on macOS", UA.macFirefox, 0, false],
    ["Chrome on Android", UA.androidChrome, 5, false],
    ["Samsung Internet", UA.androidSamsung, 5, false],
    ["Chrome on Windows", UA.windowsChrome, 0, false],
  ])("%s", (_name, userAgent, maxTouchPoints, expected) => {
    expect(isWebKitBrowser(userAgent, maxTouchPoints)).toBe(expected);
  });
});

describe("shouldRedirectInSameTab", () => {
  beforeEach(() => {
    APPLE_SAME_TAB_REDIRECT.set(true);
    OPENID_SAME_TAB_REDIRECT.set(false);
  });

  afterEach(() => {
    APPLE_SAME_TAB_REDIRECT.getFeatureFlag()?.reset();
    OPENID_SAME_TAB_REDIRECT.getFeatureFlag()?.reset();
  });

  it("redirects Sign in with Apple on WebKit", () => {
    expect(
      shouldRedirectInSameTab(APPLE_AUTH_URL, {
        userAgent: UA.iphoneSafari,
        maxTouchPoints: 5,
      }),
    ).toBe(true);
    expect(
      shouldRedirectInSameTab(APPLE_AUTH_URL, {
        userAgent: UA.macSafari,
        maxTouchPoints: 0,
      }),
    ).toBe(true);
  });

  it("keeps the popup for Sign in with Apple off WebKit", () => {
    expect(
      shouldRedirectInSameTab(APPLE_AUTH_URL, {
        userAgent: UA.macChrome,
        maxTouchPoints: 0,
      }),
    ).toBe(false);
  });

  it("keeps the popup for other providers on WebKit", () => {
    expect(
      shouldRedirectInSameTab(GOOGLE_AUTH_URL, {
        userAgent: UA.iphoneSafari,
        maxTouchPoints: 5,
      }),
    ).toBe(false);
  });

  it("keeps the popup for an unparsable authorization URL", () => {
    expect(
      shouldRedirectInSameTab("not a url", {
        userAgent: UA.iphoneSafari,
        maxTouchPoints: 5,
      }),
    ).toBe(false);
  });

  it("can be switched off for Apple on WebKit", () => {
    APPLE_SAME_TAB_REDIRECT.set(false);
    expect(
      shouldRedirectInSameTab(APPLE_AUTH_URL, {
        userAgent: UA.iphoneSafari,
        maxTouchPoints: 5,
      }),
    ).toBe(false);
  });

  it("can be forced for every provider and browser", () => {
    OPENID_SAME_TAB_REDIRECT.set(true);
    expect(
      shouldRedirectInSameTab(GOOGLE_AUTH_URL, {
        userAgent: UA.windowsChrome,
        maxTouchPoints: 0,
      }),
    ).toBe(true);
  });

  it("reads the flags from their stores", () => {
    // Guards the wiring: the flags exist and default as documented.
    APPLE_SAME_TAB_REDIRECT.getFeatureFlag()?.reset();
    OPENID_SAME_TAB_REDIRECT.getFeatureFlag()?.reset();
    expect(get(APPLE_SAME_TAB_REDIRECT)).toBe(true);
    expect(get(OPENID_SAME_TAB_REDIRECT)).toBe(false);
  });
});

describe("same-tab redirect round trip", () => {
  beforeEach(() => {
    sessionStorage.clear();
    backendCanisterConfig.openid_configs = [[appleConfig]];
    vi.useFakeTimers();
    vi.setSystemTime(new Date("2026-10-19T10:00:00Z"));
  });

  afterEach(() => {
    vi.useRealTimers();
    sessionStorage.clear();
  });

  const startRedirect = (
    options: Partial<Parameters<typeof redirectToOpenIdInSameTab>[0]> = {},
  ) => {
    const navigate = vi.fn<(url: URL) => void>();
    void redirectToOpenIdInSameTab({
      config: appleConfig,
      mode: "signin",
      navigate,
      ...options,
    });
    expect(navigate).toHaveBeenCalledTimes(1);
    const url = navigate.mock.calls[0][0];
    const state = url.searchParams.get("state");
    expect(state).not.toBeNull();
    return { url, state: state! };
  };

  const deliverCallback = (payload: unknown) => {
    // What the canister's landing page does on a same-tab return.
    sessionStorage.setItem(OPENID_CALLBACK_DATA_KEY, JSON.stringify(payload));
  };

  it("navigates to the provider with the session nonce and a return path", () => {
    const { url } = startRedirect({ returnPath: "/?from=test" });
    expect(url.origin).toBe("https://appleid.apple.com");
    expect(url.searchParams.get("nonce")).toBe("test-nonce");
    expect(url.searchParams.get("client_id")).toBe("ai.id.auth");
    expect(url.searchParams.get("response_mode")).toBe("form_post");
    expect(sessionStorage.getItem(OPENID_RETURN_PATH_KEY)).toBe("/?from=test");
    expect(sessionStorage.getItem(OPENID_WIZARD_RESUME_KEY)).not.toBeNull();
  });

  it("defaults the return path to the landing page", () => {
    startRedirect();
    expect(sessionStorage.getItem(OPENID_RETURN_PATH_KEY)).toBe("/");
  });

  it("clears markers left by another flow before starting", () => {
    sessionStorage.setItem(OPENID_AUTHORIZE_STATE_KEY, "stale");
    sessionStorage.setItem(OPENID_CALLBACK_DATA_KEY, "{}");
    startRedirect();
    expect(sessionStorage.getItem(OPENID_AUTHORIZE_STATE_KEY)).toBeNull();
    expect(sessionStorage.getItem(OPENID_CALLBACK_DATA_KEY)).toBeNull();
  });

  it("never resolves: the document is on its way out", async () => {
    const navigate = vi.fn();
    const settled = vi.fn();
    void redirectToOpenIdInSameTab({
      config: appleConfig,
      mode: "signin",
      navigate,
    }).then(settled, settled);
    await vi.advanceTimersByTimeAsync(10_000);
    expect(settled).not.toHaveBeenCalled();
  });

  it("resumes with the token, provider and mode once the callback is back", () => {
    const { state } = startRedirect({ mode: "signup" });
    expect(hasPendingOpenIdSameTabResume()).toBe(false);
    deliverCallback({ id_token: "token", state });
    expect(hasPendingOpenIdSameTabResume()).toBe(true);

    const resume = consumeOpenIdSameTabResume();

    expect(resume).toEqual({
      config: appleConfig,
      jwt: "token",
      mode: "signup",
    });
    // Single-use.
    expect(hasPendingOpenIdSameTabResume()).toBe(false);
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
    expect(sessionStorage.getItem(OPENID_RETURN_PATH_KEY)).toBeNull();
    expect(sessionStorage.getItem(OPENID_WIZARD_RESUME_KEY)).toBeNull();
    expect(sessionStorage.getItem(OPENID_CALLBACK_DATA_KEY)).toBeNull();
  });

  it("resolves the provider by issuer when the client id was rotated", () => {
    const { state } = startRedirect();
    const rotated = { ...appleConfig, client_id: "ai.id.auth.v2" };
    backendCanisterConfig.openid_configs = [[rotated]];
    deliverCallback({ id_token: "token", state });
    expect(consumeOpenIdSameTabResume()?.config).toEqual(rotated);
  });

  it("returns nothing when no sign-in is pending", () => {
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
  });

  it("returns nothing when only the callback is present (markers of another flow)", () => {
    deliverCallback({ id_token: "token", state: "whatever" });
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
    // Cleaned up all the same, so it can't leak into a later flow.
    expect(sessionStorage.getItem(OPENID_CALLBACK_DATA_KEY)).toBeNull();
  });

  it("rejects a callback whose state does not match (CSRF)", () => {
    startRedirect();
    deliverCallback({ id_token: "token", state: "forged" });
    expect(() => consumeOpenIdSameTabResume()).toThrow("Invalid state");
    // Consumed, so a retry does not throw again.
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
  });

  it("surfaces a provider-reported error", () => {
    const { state } = startRedirect();
    deliverCallback({
      error: "access_denied",
      error_description: "The user declined",
      state,
    });
    expect(() => consumeOpenIdSameTabResume()).toThrow(OAuthProviderError);
  });

  it("drops a resume that is older than the session it was bound to", () => {
    const { state } = startRedirect();
    deliverCallback({ id_token: "token", state });
    vi.setSystemTime(new Date("2026-10-19T10:06:00Z"));
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
  });

  it("drops a resume whose provider is no longer configured", () => {
    const { state } = startRedirect();
    backendCanisterConfig.openid_configs = [[]];
    deliverCallback({ id_token: "token", state });
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
  });

  it("drops a malformed stash", () => {
    sessionStorage.setItem(OPENID_WIZARD_RESUME_KEY, "not json");
    deliverCallback({ id_token: "token", state: "x" });
    expect(consumeOpenIdSameTabResume()).toBeUndefined();
  });
});
