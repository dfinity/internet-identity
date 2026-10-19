import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { II_URL } from "../utils";

/**
 * The wizard's same-tab OpenID sign-in (`openIdSameTabRedirect.ts`).
 *
 * In production it is taken for Sign in with Apple on WebKit, where the popup
 * flow can't deliver the callback. Chromium has no such provider, so the
 * `OPENID_SAME_TAB_REDIRECT` flag forces the path for every provider: the tab
 * itself goes to the IdP, the canister's /callback landing page brings it
 * back, and the wizard picks the sign-in up with the returned token.
 */

/** Persists the flag; the override beats the default in `featureFlags.ts`. */
const forceSameTabRedirect = (page: Page) =>
  page.context().addInitScript(() => {
    try {
      window.localStorage.setItem(
        "ii-localstorage-feature-flags__OPENID_SAME_TAB_REDIRECT",
        JSON.stringify(true),
      );
    } catch {
      // localStorage may be locked in some test contexts.
    }
  });

test.describe("OpenID sign-in in the same tab", () => {
  const name = "Same Tab";

  test.use({
    openIdConfig: {
      createUsers: [
        {
          claims: { name },
        },
      ],
    },
  });

  test.beforeEach(async ({ page }) => {
    await forceSameTabRedirect(page);
  });

  test("signs up and signs in on the landing page without a popup", async ({
    page,
    managePage,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await page.goto(II_URL);
    // The tab leaves for the IdP: the provider button must not open a popup.
    let popups = 0;
    page.context().on("page", () => {
      popups += 1;
    });
    await page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    await signInWithOpenId(page, openIdUsers[0].id);

    // Back on the landing page with the token: the inline picker is in
    // sign-in mode, so a fresh OpenID user gets the "not connected yet"
    // dialog — confirm sign-up to land on /manage.
    await page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", { name: new RegExp(`Welcome, ${name}\\.`) }),
    ).toBeVisible();
    expect(popups).toBe(0);

    // Sign out but keep the identity on this browser, so the landing page
    // shows the welcome-back state and the wizard only lives in its dialog.
    await managePage.signOut((confirmation) => confirmation.keepIdentity());
    // Wipe the IdP cookies so it prompts again instead of silently reusing
    // the sign-up session.
    await page.context().clearCookies();
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Add identity" }).click();
    await page
      .getByRole("dialog")
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    await signInWithOpenId(page, openIdUsers[0].id);

    // Same (iss, sub): the resumed sign-in goes straight to /manage.
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", { name: new RegExp(`Welcome, ${name}\\.`) }),
    ).toBeVisible();
    expect(popups).toBe(0);
  });
});

test.describe("Authorize with OpenID in the same tab", () => {
  const name = "Same Tab";

  test.use({
    openIdConfig: {
      createUsers: [
        {
          claims: { name },
        },
      ],
    },
    authorizeConfig: {
      protocol: "icrc25",
    },
  });

  test.beforeEach(async ({ page }) => {
    await forceSameTabRedirect(page);
  });

  test.afterEach(({ authorizedPrincipal }) => {
    expect(authorizedPrincipal?.isAnonymous()).toBe(false);
  });

  test("the wizard's provider button takes the 1-click round trip", async ({
    authorizePage,
    signInWithOpenId,
    openIdUsers,
  }) => {
    // The dapp opened II in a popup and its client is waiting on the delegation
    // request it already sent. The wizard's provider button navigates that
    // popup itself to the IdP (no inner popup, which is what fails on WebKit)
    // and comes back through the 1-click resume: II holds the client open across
    // the round trip, replays the request on the re-established channel, signs
    // up a fresh OpenID user, and authorizes the dapp — so the popup delivers
    // the delegation and closes (which the `authorizePage` fixture waits for).
    await authorizePage.page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
  });
});
