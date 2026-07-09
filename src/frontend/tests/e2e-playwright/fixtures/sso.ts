import { test as base, expect, type Page } from "@playwright/test";

/**
 * Dedicated test OpenID provider port that backs the SSO discovery
 * (two-hop) test fixture. Kept disjoint from the direct-OpenID ports so
 * a test can opt into SSO without colliding with fixtures that enumerate
 * direct providers, and so the `sso_discoverable_domains` install arg
 * (CI workflow `canister-tests.yml`) maps cleanly onto a single host.
 */
export const SSO_OPENID_PORT = 11107;
/**
 * Discovery domain (with port) that the test canister has on its SSO
 * allowlist via `sso_discoverable_domains`. Must match the value passed
 * in the install arg in `.github/workflows/canister-tests.yml`.
 */
export const SSO_DISCOVERY_DOMAIN = `localhost:${SSO_OPENID_PORT}`;

/**
 * The per-app OIDC client the test provider registers for IdP-side per-app
 * gating (mirrors `PER_APP_CLIENT_ID` in `src/test_openid_provider/index.js`).
 * A gated dapp origin maps to this client in the well-known's `app_clients`.
 */
export const SSO_PER_APP_CLIENT_ID = "ii-per-app-gated-client";

/** Configuration for the test provider's IdP-side per-app gating. */
export interface SsoGatingConfig {
  /** `origin -> client_id` map served in the well-known's `app_clients`. */
  appClients?: Record<string, string>;
  /** Default-deny an origin absent from `appClients`. */
  gateAllApps?: boolean;
  /** Cross-client-stable identifier claim (default `sub`). */
  stableIdentifierClaim?: string;
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
   * Configure the test provider's IdP-side per-app gating. Sets the
   * `app_clients` / `gate_all_apps` / `stable_identifier_claim` fields the
   * `ii-openid-configuration` well-known serves, so a test can turn a dapp
   * origin into a gated one (or default-deny). The change propagates once II's
   * discovery cache refreshes; e2e canisters use short cache windows.
   *
   * Auto-resets to un-gated defaults after each test so the change never leaks.
   */
  configureSsoGating: (config: SsoGatingConfig) => Promise<void>;
}>({
  configureSsoGating: async ({ request }, use) => {
    const post = (body: unknown) =>
      request.post(`http://${SSO_DISCOVERY_DOMAIN}/sso-config`, {
        data: body,
      });
    await use(async (config: SsoGatingConfig) => {
      const response = await post({
        app_clients: config.appClients ?? {},
        gate_all_apps: config.gateAllApps ?? false,
        stable_identifier_claim: config.stableIdentifierClaim ?? "sub",
      });
      expect(response.ok()).toBe(true);
    });
    // Reset to un-gated defaults so gating never leaks into later tests.
    await post({ app_clients: {}, gate_all_apps: false });
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
