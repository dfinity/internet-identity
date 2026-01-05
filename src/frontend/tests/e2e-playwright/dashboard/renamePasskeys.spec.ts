import { expect, test } from "@playwright/test";
import {
  addPasskeyCurrentDevice,
  clearStorage,
  createNewIdentityInII,
  dummyAuth,
  II_URL,
  renamePasskey,
} from "../utils";

const TEST_USER_NAME = "Test User";

test("User can rename the current passkey used for authentication", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Rename passkey
  await renamePasskey(page, "Chrome", "My Main Passkey");

  // Verify passkey has been renamed
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: "Chrome" }),
  ).toBeHidden();
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: "My Main Passkey" }),
  ).toBeVisible();
});

test("User can rename a newly added passkey from the same device", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Rename passkey to current passkey
  await renamePasskey(page, "Chrome", "Current passkey");

  // Add another passkey
  await addPasskeyCurrentDevice(page, dummyAuth());
  await renamePasskey(page, "Chrome", "New passkey");

  // Verify that we now have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Verify we now have both passkeys with different names
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: "Current passkey" }),
  ).toBeVisible();
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: "New passkey" }),
  ).toBeVisible();
});

test("User cannot rename passkey to an empty name nor is it renamed on cancel", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Open the rename passkey dialog
  await page
    .getByRole("listitem")
    .filter({ hasText: "Passkey" })
    .filter({ hasText: "Chrome" })
    .getByRole("button", { name: "More options" })
    .click();
  await page.getByRole("menuitem", { name: "Rename" }).click();

  // Wait for the rename dialog to open
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeVisible();

  // Expect input to be prefilled with current name
  const input = page.getByRole("textbox");
  await expect(input).toHaveValue("Chrome");

  // Expect save button to be disabled since it's unchanged
  await expect(
    page.getByRole("button", { name: "Save changes" }),
  ).toBeDisabled();

  // Clear input
  await input.clear();

  // Expect save button to be disabled since the input is empty
  await expect(
    page.getByRole("button", { name: "Save changes" }),
  ).toBeDisabled();

  // Try entering only whitespace
  await input.fill("   ");

  // Expect save button to be disabled since the input is still empty
  await expect(
    page.getByRole("button", { name: "Save changes" }),
  ).toBeDisabled();

  // Enter valid name
  await input.fill("Valid name");

  // Expect save button to be enabled now
  await expect(
    page.getByRole("button", { name: "Save changes" }),
  ).toBeEnabled();

  // Cancel the dialog
  await page.getByRole("button", { name: "Cancel" }).click();

  // Wait for the rename dialog to close
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeHidden();

  // Verify the original name is still there
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: "Chrome" }),
  ).toBeVisible();
});
