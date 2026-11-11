import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import type { OpenIdUser } from "../fixtures/openid";
import {
  SSO_ALIAS_DISCOVERY_DOMAIN,
  SSO_DISCOVERY_DOMAIN,
  SSO_OPENID_PORT,
} from "../fixtures/sso";
import {
  II_URL,
  setTestAppProtocol,
  setTestAppProvider,
  TEST_APP_URL,
} from "../utils";

// An SSO credential signs in through the discovery domain it was linked with:
// the canister scopes the anchor lookup by the stored `sso_domain` stamp, while
// registration uniqueness spans all domains. Signing in through another domain
// that reaches the same IdP and account (`SSO_ALIAS_DISCOVERY_DOMAIN` points at
// the `SSO_DISCOVERY_DOMAIN` provider) therefore neither finds the identity nor
// may create a new one; the canister answers `SsoDomainMismatch`.
//
// Interactive flows, where the user typed the domain, get a guided view naming
// the registered domain (with a retry through it, and recovery). The 1-click
// `?sso=` flow, where the *dapp* chose the domain, must only report an error —
// a guided "sign in with X" / "recover" screen there would let a malicious dapp
// script the "fix" for a confused user.

const name = "John Doe";

type SsoEntryMode = "signin" | "signup" | "both";

interface SsoFixtures {
  page: Page;
  managePage: { signOut(): Promise<void> };
  openSsoPopup: (
    authPage: Page,
    domain?: string,
    mode?: SsoEntryMode,
  ) => Promise<Page>;
  signInWithOpenId: (page: Page, userId: string) => Promise<void>;
  openIdUsers: OpenIdUser[];
}

/**
 * Register the test user's identity through `SSO_DISCOVERY_DOMAIN` on the
 * landing page, then sign out and wipe local storage and the IdP cookies so
 * the flow under test starts fresh: no last-used row, and the IdP prompts for
 * the login again instead of silently reusing the session.
 */
const registerThroughSsoDomain = async ({
  page,
  managePage,
  openSsoPopup,
  signInWithOpenId,
  openIdUsers,
}: SsoFixtures): Promise<void> => {
  await page.goto(II_URL);
  const popup = await openSsoPopup(page, SSO_DISCOVERY_DOMAIN, "signin");
  const closed = popup.waitForEvent("close", { timeout: 15_000 });
  await signInWithOpenId(popup, openIdUsers[0].id);
  await closed;
  // The landing page picker is mode="signin": a new SSO user is offered a
  // "Sign up" prompt before the registration settles on /manage.
  await page
    .getByRole("dialog")
    .getByRole("button", { name: "Sign up" })
    .click();
  await page.waitForURL(II_URL + "/manage");

  await managePage.signOut();
  await page.evaluate(() => window.localStorage.clear());
  await page.context().clearCookies();
};

/**
 * Sign in through the alias domain on the landing page and return the dialog
 * that shows the domain-mismatch view.
 */
const signInThroughAliasDomain = async ({
  page,
  openSsoPopup,
  signInWithOpenId,
  openIdUsers,
}: SsoFixtures) => {
  await page.goto(II_URL);
  const popup = await openSsoPopup(page, SSO_ALIAS_DISCOVERY_DOMAIN, "signin");
  const closed = popup.waitForEvent("close", { timeout: 15_000 });
  await signInWithOpenId(popup, openIdUsers[0].id);
  await closed;

  const dialog = page.getByRole("dialog");
  await expect(
    dialog.getByRole("heading", { name: "Linked through another SSO domain" }),
  ).toBeVisible();
  // The registered domain comes from the canister, not from the typed one.
  await expect(
    dialog.getByText(`Linked through ${SSO_DISCOVERY_DOMAIN}`, { exact: true }),
  ).toBeVisible();
  // No sign-up: registration would reject the credential as a duplicate.
  await expect(dialog.getByRole("button", { name: "Sign up" })).toHaveCount(0);
  return dialog;
};

test.describe("SSO sign-in through another discovery domain", () => {
  test.use({
    openIdConfig: {
      defaultPort: SSO_OPENID_PORT,
      createUsers: [{ claims: { name } }],
    },
  });

  test("manual sign-in offers the registered domain and recovery", async ({
    page,
    managePage,
    openSsoPopup,
    signInWithOpenId,
    openIdUsers,
  }) => {
    const fixtures = {
      page,
      managePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    };
    await registerThroughSsoDomain(fixtures);
    const dialog = await signInThroughAliasDomain(fixtures);

    await expect(
      dialog.getByRole("button", {
        name: `Sign in with ${SSO_DISCOVERY_DOMAIN}`,
      }),
    ).toBeVisible();

    // Recovery sits behind the "Why am I seeing this?" details.
    await dialog.getByText("Why am I seeing this?").click();
    await dialog.getByRole("button", { name: "Recover" }).click();
    await page.waitForURL(II_URL + "/recovery");
  });

  test("manual sign-in through the registered domain reaches the identity", async ({
    page,
    managePage,
    openSsoPopup,
    signInWithOpenId,
    openIdUsers,
  }) => {
    const fixtures = {
      page,
      managePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    };
    await registerThroughSsoDomain(fixtures);
    const dialog = await signInThroughAliasDomain(fixtures);

    // The button is enabled once II has resolved discovery for the registered
    // domain; the retry ceremony must then prompt at the IdP again, so drop the
    // IdP session cookie the mismatching sign-in just left behind.
    const retry = dialog.getByRole("button", {
      name: `Sign in with ${SSO_DISCOVERY_DOMAIN}`,
    });
    await expect(retry).toBeEnabled({ timeout: 30_000 });
    await page.context().clearCookies({ domain: "localhost" });
    const retryPopupPromise = page.context().waitForEvent("page");
    await retry.click();
    const retryPopup = await retryPopupPromise;
    const retryClosed = retryPopup.waitForEvent("close", { timeout: 15_000 });
    await signInWithOpenId(retryPopup, openIdUsers[0].id);
    await retryClosed;

    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", { name: new RegExp(`Welcome, ${name}\\.`) }),
    ).toBeVisible();
  });

  test("1-click sign-in only reports an error, without guidance or recovery", async ({
    page,
    managePage,
    openSsoPopup,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await registerThroughSsoDomain({
      page,
      managePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    });

    // The dapp picks the (alias) domain via `?sso=`; the II popup redirects to
    // the IdP and resumes with the JWT on its own, without the wizard.
    await page.goto(TEST_APP_URL);
    await setTestAppProvider(
      page,
      `${II_URL}/authorize?sso=${encodeURIComponent(SSO_ALIAS_DISCOVERY_DOMAIN)}`,
    );
    await setTestAppProtocol(page, true);
    await expect(page.locator("#principal")).toBeHidden();
    const popupPromise = page.context().waitForEvent("page");
    await page.getByRole("button", { name: "Sign In" }).click();
    const popup = await popupPromise;
    await signInWithOpenId(popup, openIdUsers[0].id);

    // A plain error toast, and none of the guided view's affordances.
    await expect(
      popup.getByText("This account is linked through a different SSO domain"),
    ).toBeVisible({ timeout: 15_000 });
    await expect(
      popup.getByRole("heading", { name: "Linked through another SSO domain" }),
    ).toHaveCount(0);
    await expect(popup.getByRole("button", { name: "Recover" })).toHaveCount(0);
    await expect(
      popup.getByRole("button", {
        name: `Sign in with ${SSO_DISCOVERY_DOMAIN}`,
      }),
    ).toHaveCount(0);
    // Nothing was authorized for the dapp.
    await expect(page.locator("#principal")).toBeHidden();
  });
});
