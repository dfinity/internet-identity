import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { II_URL } from "../../utils";

test.describe("Dashboard Navigation", () => {
  test("User can register, sign in, access the dashboard and navigate to security page", async ({
    page,
    identities,
    signInWithIdentity,
    managePage,
  }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    // Navigate to access methods
    const menuButton = page.getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
    await page.getByRole("link", { name: "Access and recovery" }).click();

    // Check that we have one passkey listed
    const passkey = await page.getByText("Unknown");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });

  test.describe("multiple identities", () => {
    test.use({
      identityConfig: {
        createIdentities: [{ name: "Test 1" }, { name: "Test 2" }],
      },
    });

    test.beforeEach(
      async ({
        page,
        managePage,
        identities,
        signInWithIdentity,
        removeAuthenticatorForIdentity,
      }) => {
        // Sign in with both identities to add them both to switcher
        await page.goto(II_URL);
        await signInWithIdentity(page, identities[1].identityNumber);
        await managePage.signOut();
        await removeAuthenticatorForIdentity(
          page,
          identities[1].identityNumber,
        );
        await signInWithIdentity(page, identities[0].identityNumber);
        await managePage.signOut();
        await removeAuthenticatorForIdentity(
          page,
          identities[0].identityNumber,
        );
      },
    );

    test("User can switch between identities", async ({
      page,
      managePage,
      identities,
      addAuthenticatorForIdentity,
      removeAuthenticatorForIdentity,
    }) => {
      // Sign in to dashboard with first identity
      await page.goto(II_URL);
      await addAuthenticatorForIdentity(page, identities[0].identityNumber);
      await page.getByRole("button", { name: "Switch identity" }).click();
      await page
        .getByRole("button", { name: "Manage your Internet Identity" })
        .click();

      // Verify we're at the dashboard and signed in as the first identity
      await managePage.assertVisible();
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${identities[0].name}!`),
        }),
      ).toBeVisible();

      // Navigate to access methods
      const menuButton = page.getByRole("button", { name: "Open menu" });
      if (await menuButton.isVisible()) {
        await menuButton.click();
      }
      await page.getByRole("link", { name: "Access and recovery" }).click();

      // Switch to second identity
      await removeAuthenticatorForIdentity(page, identities[0].identityNumber);
      await addAuthenticatorForIdentity(page, identities[1].identityNumber);
      await page.getByRole("button", { name: "Switch identity" }).click();
      await page.getByRole("button", { name: identities[1].name }).click();

      // Verify we're back at the dashboard homepage and signed in as the second identity
      await managePage.assertVisible();
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${identities[1].name}!`),
        }),
      ).toBeVisible();
    });
  });
});
