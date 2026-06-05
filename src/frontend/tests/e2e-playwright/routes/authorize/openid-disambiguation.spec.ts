import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, II_URL } from "../../utils";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";

const DEFAULT_USER_NAME = "John Doe";

// OIDC sign-in succeeds for existing II — no disambiguation dialog
test.describe("OIDC sign-in for existing II at /authorize", () => {
  const name = DEFAULT_USER_NAME;

  test.use({
    openIdConfig: {
      defaultPort: DEFAULT_OPENID_PORT,
      createUsers: [
        {
          claims: { name },
        },
      ],
    },
  });

  test("OIDC sign-in with known identity proceeds without disambiguation dialog", async ({
    page,
    managePage,
    signInWithOpenId,
    openIdUsers,
  }) => {
    // Register the user first
    await page.goto(II_URL);
    const signUpPopupPromise = page.context().waitForEvent("page");
    await page
      .getByRole("button", { name: openIdUsers[0].issuer.name })
      .click();
    const signUpPopup = await signUpPopupPromise;
    const signUpClosePromise = signUpPopup.waitForEvent("close", {
      timeout: 15_000,
    });
    await signInWithOpenId(signUpPopup, openIdUsers[0].id);
    await signUpClosePromise;
    // Fresh OIDC user on the homepage surfaces IdentityNotConnectedDialog
    // — confirm sign-up to land on /manage.
    await page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await page.waitForURL(II_URL + "/manage");
    await managePage.signOut((c) => c.keepIdentity());
    await page.context().clearCookies();

    // Now sign in via /authorize — known (iss, sub) goes straight to ContinueView.
    // keepIdentity() left the last-used entry in localStorage, so the picker is
    // skipped. ContinueView still calls AuthLastUsedFlow.authenticate on Continue
    // click, which opens an OIDC popup (mediation: "optional").
    await authorize(page, async (authPage) => {
      await expect(
        authPage.getByRole("heading", { name: /^Continue to/ }),
      ).toBeVisible();
      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeHidden();
      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeHidden();

      const signInPopupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
      const signInPopup = await signInPopupPromise;
      const signInClosePromise = signInPopup.waitForEvent("close", {
        timeout: 15_000,
      });
      await signInWithOpenId(signInPopup, openIdUsers[0].id);
      await signInClosePromise;
    });
  });
});
