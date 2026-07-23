import { test as base, expect, type Page } from "@playwright/test";

/**
 * Dedicated test OpenID provider port that backs the SSO discovery
 * (two-hop) test fixture. Kept disjoint from the direct-OpenID ports so
 * a test can opt into SSO without colliding with fixtures that enumerate
 * direct providers. Discovery reaches it over `http` because the test canister
 * runs with `sso_allow_insecure_discovery` (loopback http; see dev-e2e-setup).
 */
export const SSO_OPENID_PORT = 11107;
/** Discovery domain (with port) the SSO tests point at (the loopback provider). */
export const SSO_DISCOVERY_DOMAIN = `localhost:${SSO_OPENID_PORT}`;

/**
 * A second discovery domain for the same test provider (`127.0.0.1`, same port)
 * used only by the gating tests, so their config lands in a discovery-cache
 * entry the un-gated tests never touch (II caches discovery per domain ~1h with
 * no force-refresh).
 */
export const SSO_GATING_DISCOVERY_DOMAIN = `127.0.0.1:${SSO_OPENID_PORT}`;

/**
 * Entra-style provider: pairwise `sub` (different per OIDC client) plus a stable
 * `oid` claim. Backs the non-`sub` gating tests where the identity must be
 * bridged via `oid`. Its own port/domain so its pairwise config never touches
 * the public-`sub` provider the other tests rely on (see scripts/dev-e2e-setup).
 */
export const SSO_ENTRA_OPENID_PORT = 11108;
export const SSO_ENTRA_DISCOVERY_DOMAIN = `localhost:${SSO_ENTRA_OPENID_PORT}`;
/** The SSO display name the Entra instance serves (see dev-e2e-setup). */
export const SSO_ENTRA_NAME = "Entra SSO";

/**
 * The per-app OIDC client id the test provider registers for gating; a gated
 * origin maps to it in the well-known's `app_clients`.
 */
export const SSO_PER_APP_CLIENT_ID = "ii-per-app-gated-client";

/** Configuration for the test provider's per-app gating. */
export interface SsoGatingConfig {
  /** `origin -> client_id` map served in the well-known's `app_clients`. */
  appClients?: Record<string, string>;
  /** Default-deny an origin absent from `appClients`. */
  gateAllApps?: boolean;
  /** Cross-client-stable identifier claim (default `sub`). */
  stableIdentifierClaim?: string;
  /**
   * Which provider to configure (default the public-`sub` one). Set to
   * {@link SSO_ENTRA_DISCOVERY_DOMAIN} for the pairwise/`oid` tests.
   */
  domain?: string;
}

type SsoEntryMode = "signin" | "signup" | "both";

const ssoEntryLabel = (mode: SsoEntryMode): string =>
  mode === "signin"
    ? "Sign in with SSO"
    : mode === "signup"
      ? "Sign up with SSO"
      : "Continue with SSO";

export const test = base.extend<{
  /**
   * Drives only the II-side of the SSO entry: clicks the SSO entry
   * button (label varies with the picker's mode), types the discovery
   * domain, waits for hop-1 + hop-2 to resolve, then clicks Continue
   * and returns the IdP popup. The popup itself is just
   * `test_openid_provider`, so callers follow up with
   * `signInWithOpenId` from the OpenID fixture the same way
   * direct-OpenID tests do — there's nothing SSO-specific about the
   * IdP page.
   */
  openSsoPopup: (
    authPage: Page,
    domain?: string,
    mode?: SsoEntryMode,
  ) => Promise<Page>;
  /**
   * Configure the test provider's per-app gating (`app_clients` /
   * `gate_all_apps` / `stable_identifier_claim` in the well-known).
   * Auto-resets to un-gated defaults after each test.
   */
  configureSsoGating: (config: SsoGatingConfig) => Promise<void>;
}>({
  configureSsoGating: async ({ request }, use) => {
    const post = (domain: string, body: unknown) =>
      request.post(`http://${domain}/sso-config`, { data: body });
    const configured = new Set<string>();
    await use(async (config: SsoGatingConfig) => {
      const domain = config.domain ?? SSO_DISCOVERY_DOMAIN;
      configured.add(domain);
      const response = await post(domain, {
        app_clients: config.appClients ?? {},
        gate_all_apps: config.gateAllApps ?? false,
        stable_identifier_claim: config.stableIdentifierClaim ?? "sub",
      });
      expect(response.ok()).toBe(true);
    });
    // Reset every configured provider so gating never leaks into later tests.
    for (const domain of configured) {
      await post(domain, { app_clients: {}, gate_all_apps: false });
    }
  },
  // eslint-disable-next-line no-empty-pattern
  openSsoPopup: async ({}, use) => {
    await use(
      async (
        authPage: Page,
        domain: string = SSO_DISCOVERY_DOMAIN,
        mode: SsoEntryMode = "both",
      ) => {
        await authPage
          .getByRole("button", { name: ssoEntryLabel(mode) })
          .click();
        await authPage
          .getByRole("textbox", { name: "Company domain" })
          .fill(domain);
        // Continue is disabled until the canister resolves SSO discovery for
        // the domain and stashes a `preparedResult`. The button label flips
        // between "Checking..." and "Continue" — wait on the latter to know
        // discovery's done.
        const continueButton = authPage.getByRole("button", {
          name: "Continue",
          exact: true,
        });
        await expect(continueButton).toBeEnabled({ timeout: 30_000 });
        const popupPromise = authPage.context().waitForEvent("page");
        await continueButton.click();
        return await popupPromise;
      },
    );
  },
});
