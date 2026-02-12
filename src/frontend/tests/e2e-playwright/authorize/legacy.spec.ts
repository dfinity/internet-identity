import { test, expect } from "@playwright/test";
import {
  II_URL,
  LEGACY_II_URL,
  ALT_LEGACY_II_URL,
  TEST_APP_URL,
  dummyAuth,
  authorizeWithUrl,
} from "../utils";
[LEGACY_II_URL, ALT_LEGACY_II_URL].forEach((legacyURL) => {
  test.describe(`Legacy domain ${legacyURL}`, () => {
    test(`sees upgrade banner during authentication`, async ({ page }) => {
      const auth = dummyAuth();
      await authorizeWithUrl(
        page,
        TEST_APP_URL,
        legacyURL,
        async (authPage) => {
          // Assert that we've been redirected to non-legacy domain
          await expect(authPage).toHaveURL((url) => url.origin === II_URL);
          // Assert that the user is informed about the upgrade
          await expect(
            authPage.getByRole("heading", {
              name: "has moved to the new Internet Identity",
            }),
          ).toBeVisible();
          // Create new identity and continue to app
          await authPage
            .getByRole("button", { name: "Continue with Passkey" })
            .click();
          await authPage
            .getByRole("button", { name: "Create new identity" })
            .click();
          await authPage.getByLabel("Identity name").fill("Test");
          auth(authPage);
          await authPage
            .getByRole("button", { name: "Create identity" })
            .click();
          await authPage
            .getByRole("button", { name: "Continue", exact: true })
            .click();
        },
      );
    });
  });
});
