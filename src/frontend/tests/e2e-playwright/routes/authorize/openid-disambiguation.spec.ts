import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, addVirtualAuthenticator, II_URL } from "../../utils";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";

const DEFAULT_USER_NAME = "John Doe";

// OIDC cancellation → "Sign-in was canceled" toast
test.describe("OIDC cancel toast", () => {
  test.use({
    openIdConfig: {
      defaultPort: DEFAULT_OPENID_PORT,
      createUsers: [
        {
          claims: { name: DEFAULT_USER_NAME },
        },
      ],
    },
  });

  test("closing the OIDC popup without signing in shows 'Sign-in was canceled' toast", async ({
    page,
    openIdUsers,
  }) => {
    await authorize(page, async (authPage) => {
      // Click the OpenID provider button — popup opens
      const popupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const popup = await popupPromise;

      // Close the popup without signing in (user cancels)
      await popup.close();

      // The cancel toast should appear
      await expect(
        authPage
          .getByRole("status")
          .filter({ hasText: "Sign-in was canceled" }),
      ).toBeVisible({ timeout: 5_000 });

      // Complete the flow so authorize utility can finish
      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Sign up", exact: true })
        .click();
      await authPage
        .getByRole("button", { name: "Sign up with passkey" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

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

    // Now sign in via /authorize — known (iss, sub) should go straight to ContinueView
    await authorize(page, async (authPage) => {
      const signInPopupPromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();
      const signInPopup = await signInPopupPromise;
      const signInClosePromise = signInPopup.waitForEvent("close", {
        timeout: 15_000,
      });
      await signInWithOpenId(signInPopup, openIdUsers[0].id);
      await signInClosePromise;

      // No disambiguation dialog — ContinueView should be visible immediately
      await expect(
        authPage.getByRole("heading", { name: "Create your Identity" }),
      ).toBeHidden();
      await expect(
        authPage.getByRole("heading", { name: "Already connected" }),
      ).toBeHidden();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});
