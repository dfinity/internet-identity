import { expect, test } from "@playwright/test";
import {
  clearStorage,
  createNewIdentityInII,
  dummyAuth,
  II_URL,
  addPasskeyCurrentDevice,
} from "../utils";

const TEST_USER_NAME = "Test User";

test("User can remove a passkey when they have multiple access methods", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  // Start the "add passkey" flow to create a second passkey
  await addPasskeyCurrentDevice(page, dummyAuth());

  // Verify that we now have two passkeys
  await expect(page.getByText("Chrome")).toHaveCount(2);

  // Wait for remove buttons to appear (they only show when there are multiple access methods)
  await expect(page.getByLabel("Remove passkey")).toBeVisible();

  // Click the remove button for the passkey (not the current one)
  // Label for current one is "Remove current passkey"
  const removeButtons = page.getByLabel("Remove passkey");
  await expect(removeButtons).toHaveCount(1);
  await removeButtons.click();

  const removePasskeyDialog = page.getByRole("dialog");
  // Verify the remove dialog opens
  await expect(
    removePasskeyDialog.getByRole("heading", {
      level: 1,
      name: "Are you sure?",
    }),
  ).toBeVisible();

  // Verify the dialog shows the standard warning (not the current access method warning)
  await expect(
    removePasskeyDialog.getByText(
      "Removing this passkey means you won't be able to use it to sign in anymore. You can always add a new one later.",
    ),
  ).toBeVisible();

  // Verify the current access method warning is NOT shown
  await expect(
    removePasskeyDialog.getByText(
      "As you are currently signed in with this passkey, you will be signed out.",
    ),
  ).not.toBeVisible();

  // Confirm removal
  await removePasskeyDialog
    .getByRole("button", { name: "Remove passkey" })
    .click();

  // Verify the dialog closes and we now have only one passkey
  await expect(removePasskeyDialog).not.toBeVisible();
  await expect(page.getByText("Chrome")).toHaveCount(1);

  // Verify we're still logged in and at the dashboard
  await expect(page).toHaveURL(II_URL + "/manage");
  await expect(
    page.getByRole("heading", {
      name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
    }),
  ).toBeVisible();
});

test("User cannot remove passkey if they only have one access method", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  // Verify that the remove button is not visible when there's only one access method
  await expect(page.getByLabel("Remove current passkey")).not.toBeVisible();

  // Verify that the rename button is still visible (to ensure we're looking at the right area)
  await expect(page.getByLabel("Rename current passkey")).toBeVisible();
});

test("User is logged out after removing the passkey they used to authenticate", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  await addPasskeyCurrentDevice(page, dummyAuth());

  // Verify that we now have two passkeys
  await expect(page.getByText("Chrome")).toHaveCount(2);

  // Click the remove button for the current passkey (the one used for authentication)
  const removeButtons = page.getByLabel("Remove current passkey");
  await expect(removeButtons).toHaveCount(1);
  await removeButtons.click();

  // Verify the remove dialog opens
  const removePasskeyDialog = page.getByRole("dialog");
  await expect(
    removePasskeyDialog.getByRole("heading", {
      level: 1,
      name: "Are you sure?",
    }),
  ).toBeVisible();

  // Verify the dialog shows both the standard warning AND the current access method warning
  await expect(
    removePasskeyDialog.getByText(
      "Removing this passkey means you won't be able to use it to sign in anymore. You can always add a new one later.",
    ),
  ).toBeVisible();

  await expect(
    removePasskeyDialog.getByText(
      "As you are currently signed in with this passkey, you will be signed out.",
    ),
  ).toBeVisible();

  // Confirm removal
  await removePasskeyDialog
    .getByRole("button", { name: "Remove passkey" })
    .click();

  // Verify the user is logged out and redirected to the login page
  // The URL should change from /manage to the root or login page
  await page.waitForURL(II_URL);

  // Verify we're back at the login screen without selectable identity
  await expect(
    page.getByRole("button", { name: "Continue with Passkey" }),
  ).toBeVisible();

  // Verify we're no longer at the dashboard
  await expect(
    page.getByRole("heading", {
      name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
    }),
  ).not.toBeVisible();
});

test("User can cancel passkey removal", async ({ page }) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  await addPasskeyCurrentDevice(page, dummyAuth());

  // Verify that we now have two passkeys
  await expect(page.getByText("Chrome")).toHaveCount(2);

  // Wait for remove buttons to appear (they only show when there are multiple access methods)
  await expect(page.getByLabel("Remove passkey")).toBeVisible();

  // Click the remove button for the passkey
  const removeButtons = page.getByLabel("Remove passkey");
  await expect(removeButtons).toHaveCount(1);
  await removeButtons.click();

  // Verify the remove dialog opens
  const removePasskeyDialog = page.getByRole("dialog");
  await expect(
    removePasskeyDialog.getByRole("heading", {
      level: 1,
      name: "Are you sure?",
    }),
  ).toBeVisible();

  // Cancel the removal
  await removePasskeyDialog.getByRole("button", { name: "Cancel" }).click();

  // Verify the dialog closes and we still have both passkeys
  await expect(
    removePasskeyDialog.getByRole("heading", {
      level: 1,
      name: "Are you sure?",
    }),
  ).not.toBeVisible();

  await expect(page.getByText("Chrome")).toHaveCount(2);

  // Verify we're still logged in and at the dashboard
  await expect(page).toHaveURL(II_URL + "/manage");
  await expect(
    page.getByRole("heading", {
      name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
    }),
  ).toBeVisible();
});
