import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { IdentityWizard } from "../../fixtures/identity";
import {
  addVirtualAuthenticator,
  authorizeWithUrl,
  expectTextFullyVisible,
  II_URL,
  serveAppMetadata,
  TEST_APP_LONG_ORIGIN_URL,
  TEST_APP_URL,
} from "../../utils";

/** A phone-width window, tall enough to show the whole sign-in panel. */
const NARROW_VIEWPORT = { width: 390, height: 1200 };
/** An app name of the 40 characters `MAX_APP_NAME_LENGTH` allows
 *  (`src/frontend/src/lib/utils/appMetadata.ts`), with nowhere to break. */
const UNBROKEN_APP_NAME = "Averyveryverylongappnamewithnowheretobrk";
const DEFAULT_USER_NAME = "John Doe";

test.describe("Authorize screens for an app with a long origin or name", () => {
  test("keeps the sign-in screen inside a narrow window for an app with a long origin", async ({
    page,
  }) => {
    await authorizeWithUrl(
      page,
      TEST_APP_LONG_ORIGIN_URL,
      II_URL,
      async (authPage) => {
        await authPage.setViewportSize(NARROW_VIEWPORT);
        const createButton = authPage.getByRole("button", {
          name: "Create",
          exact: true,
        });
        // Asserted on position rather than through a click: Playwright scrolls
        // a clipped button into view before clicking it.
        await expect(createButton).toBeInViewport({ ratio: 1 });
        await expectTextFullyVisible(authPage.getByText("to continue to"));

        await addVirtualAuthenticator(authPage);
        await new IdentityWizard(authPage).signUp(DEFAULT_USER_NAME);
        await authPage
          .getByRole("button", { name: "Continue", exact: true })
          .click();
      },
    );
  });

  test("wraps an unbroken app name of the maximum length in the 'Continue to' heading", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    await serveAppMetadata(page.context(), TEST_APP_URL, {
      name: UNBROKEN_APP_NAME,
    });
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await page.waitForURL(II_URL + "/manage");

    await authorizeWithUrl(page, TEST_APP_URL, II_URL, async (authPage) => {
      await authPage.setViewportSize(NARROW_VIEWPORT);
      const heading = authPage.getByRole("heading", {
        name: `Continue to ${UNBROKEN_APP_NAME}`,
      });
      const continueButton = authPage.getByRole("button", {
        name: "Continue",
        exact: true,
      });
      await expect(heading).toBeVisible();
      await expectTextFullyVisible(heading);
      await expect(continueButton).toBeInViewport({ ratio: 1 });

      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await continueButton.click();
    });
  });
});
