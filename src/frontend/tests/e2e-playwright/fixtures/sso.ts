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

export const test = base.extend<{
  /**
   * Drives only the II-side of the SSO entry: clicks "Continue with
   * SSO", types the discovery domain, waits for hop-1 + hop-2 to
   * resolve, then clicks Continue and returns the IdP popup. The popup
   * itself is just `test_openid_provider`, so callers follow up with
   * `signInWithOpenId` from the OpenID fixture the same way
   * direct-OpenID tests do — there's nothing SSO-specific about the
   * IdP page.
   */
  openSsoPopup: (authPage: Page, domain?: string) => Promise<Page>;
}>({
  // Playwright requires fixture functions to start with an object
  // destructuring pattern even when nothing is consumed; the empty
  // pattern would normally trip the no-empty-pattern lint rule.
  // eslint-disable-next-line no-empty-pattern
  openSsoPopup: async ({}, use) => {
    await use(async (authPage: Page, domain: string = SSO_DISCOVERY_DOMAIN) => {
      await authPage.getByRole("button", { name: "Continue with SSO" }).click();
      await authPage
        .getByRole("textbox", { name: "Company domain" })
        .fill(domain);
      // Continue is disabled until both `add_discoverable_oidc_config`
      // and the two-hop discovery resolve and stash a `preparedResult`.
      // The button label flips between "Checking..." and "Continue" —
      // wait on the latter to know discovery's done.
      const continueButton = authPage.getByRole("button", {
        name: "Continue",
        exact: true,
      });
      await expect(continueButton).toBeEnabled({ timeout: 30_000 });
      const popupPromise = authPage.context().waitForEvent("page");
      await continueButton.click();
      return await popupPromise;
    });
  },
});
