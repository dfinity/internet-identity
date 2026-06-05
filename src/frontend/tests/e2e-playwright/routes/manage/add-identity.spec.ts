import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { II_URL } from "../../utils";

// Covers the "Add identity" CTA in the IdentitySwitcher popover (manage layout).
// The popover opens isAuthDialogOpen → AuthWizard(mode="signin"); the sign-up
// toggle opens isCreateIdentityDialogOpen → AuthWizard(mode="signup"), and the
// sign-in toggle inside the sign-up dialog re-opens the sign-in dialog (because
// signUpOpenedFromSignInModal is true).

test.describe("Add identity from manage layout header popover", () => {
  test.use({
    identityConfig: {
      createIdentities: [{ name: "Alice" }],
    },
  });

  test.beforeEach(async ({ page, identities, signInWithIdentity }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await page.waitForURL(II_URL + "/manage");
  });

  test("clicking Add identity opens sign-in modal with correct heading and subtitle", async ({
    page,
  }) => {
    // Open the identity popover
    await page.getByRole("button", { name: "Switch identity" }).click();

    // Click "Add identity" in the popover
    await page.getByRole("button", { name: "Add identity" }).click();

    // Sign-in modal should appear with the disambiguation heading
    await expect(
      page.getByRole("heading", { name: "Sign in", exact: true }),
    ).toBeVisible();
    await expect(page.getByText("Choose method to continue")).toBeVisible();
  });

  test("clicking Sign up inside sign-in modal opens sign-up modal", async ({
    page,
  }) => {
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Add identity" }).click();
    await expect(
      page.getByRole("heading", { name: "Sign in", exact: true }),
    ).toBeVisible();

    // Toggle to sign-up
    await page.getByRole("button", { name: "Sign up", exact: true }).click();
    // Sign-up dialog heading should appear
    await expect(
      page.getByRole("heading", {
        name: "Create another identity",
        exact: true,
      }),
    ).toBeVisible();
  });

  test("clicking Sign in inside sign-up modal returns to sign-in modal when opened from sign-in modal", async ({
    page,
  }) => {
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Add identity" }).click();
    await expect(
      page.getByRole("heading", { name: "Sign in", exact: true }),
    ).toBeVisible();

    // Toggle to sign-up
    await page.getByRole("button", { name: "Sign up", exact: true }).click();
    await expect(
      page.getByRole("heading", {
        name: "Create another identity",
        exact: true,
      }),
    ).toBeVisible();

    // Toggle back to sign-in
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expect(
      page.getByRole("heading", { name: "Sign in", exact: true }),
    ).toBeVisible();
  });
});
