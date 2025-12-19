import { expect, test } from "@playwright/test";
import {
  clearStorage,
  createNewIdentityInII,
  dummyAuth,
  II_URL,
  addPasskeyCurrentDevice,
  renamePasskey,
  removePasskey,
} from "../utils";

const TEST_USER_NAME = "Test User";

test("User can remove a passkey when they have multiple access methods", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
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

  // Rename current passkey to old passkey
  await renamePasskey(page, "Chrome", "Current passkey");

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Start the "add passkey" flow to create a second passkey
  await addPasskeyCurrentDevice(page, dummyAuth());
  await renamePasskey(page, "Chrome", "New passkey");

  // Verify that we now have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Remove new passkey
  await removePasskey(page, "New passkey", false);

  // Verify that we now have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Verify we're still logged in and at the dashboard
  await expect(page).toHaveURL(II_URL + "/manage/access");
});

test("User cannot remove passkey if they only have one access method", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
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

  // Verify that the remove button is not visible when there's only one access method
  await page
    .getByRole("listitem")
    .filter({ hasText: "Chrome" })
    .getByRole("button", { name: "More options" })
    .click();
  await expect(page.getByRole("menuitem", { name: "Remove" })).toBeHidden();

  // Verify that the rename button is still visible (to ensure we're looking at the right area)
  await expect(page.getByRole("menuitem", { name: "Rename" })).toBeVisible();
});

test("User is logged out after removing the passkey they used to authenticate", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
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

  // Rename passkey to current passkey
  await renamePasskey(page, "Chrome", "Current passkey");

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Start the "add passkey" flow to create a second passkey
  await addPasskeyCurrentDevice(page, dummyAuth());
  await renamePasskey(page, "Chrome", "New passkey");

  // Verify that we now have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Remove current passkey
  await removePasskey(page, "Current passkey", true);

  // Verify the user is logged out and redirected to the login page
  // The URL should change from /manage to the root or login page
  await page.waitForURL(II_URL + "/login");

  // Verify we're back at the login screen without selectable identity
  await expect(
    page.getByRole("button", { name: "Continue with passkey" }),
  ).toBeVisible();

  // Verify we're no longer at the dashboard
  await expect(
    page.getByRole("heading", {
      name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
    }),
  ).toBeHidden();
});

test("User can cancel passkey removal", async ({ page }) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("link", { name: "Manage Identity" }).click();
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

  // Rename passkey to current passkey
  await renamePasskey(page, "Chrome", "Current passkey");

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  await addPasskeyCurrentDevice(page, dummyAuth());

  // Verify that we now have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Open the remove passkey dialog for the current passkey
  await page
    .getByRole("listitem")
    .filter({ hasText: "Current passkey" })
    .getByRole("button", { name: "More options" })
    .click();
  await page.getByRole("menuitem", { name: "Remove" }).click();

  // Wait for the remove dialog to open
  await expect(
    page.getByRole("heading", { name: "Are you sure?" }),
  ).toBeVisible();
  await expect(
    page.getByText(
      "Removing this passkey means you won't be able to use it to sign in anymore. You can always add a new one later.",
    ),
  ).toBeVisible();

  // Cancel the removal
  await page.getByRole("button", { name: "Cancel" }).click();

  // Wait for the remove dialog to close
  await expect(
    page.getByRole("heading", { name: "Are you sure?" }),
  ).toBeHidden();

  // Verify that we still have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Verify we're still logged in and at the dashboard
  await expect(page).toHaveURL(II_URL + "/manage/access");
});
