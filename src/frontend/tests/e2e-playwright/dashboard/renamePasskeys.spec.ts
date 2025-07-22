import { expect, test } from "@playwright/test";
import {
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

  // Click the rename button for the passkey
  await page.getByLabel("Rename current Passkey").click();

  // Verify the rename dialog opens
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeVisible();

  // Verify the input is pre-filled with current name
  const input = page.getByRole("textbox");
  await expect(input).toHaveValue("Chrome");

  // Change the name
  await input.clear();
  const passkeyName = "My Main Passkey";
  await input.fill(passkeyName);

  // Save the changes
  await page.getByRole("button", { name: "Save" }).click();

  // Verify the dialog closes and the passkey now shows the new name
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).not.toBeVisible();
  await expect(page.getByText(passkeyName)).toBeVisible();
  await expect(page.getByText("Chrome")).not.toBeVisible();
});

test("User can rename a newly added passkey from the same device", async ({
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

  // Start the "add passkey" flow
  await page.getByRole("button", { name: "Add" }).click();
  await page.getByRole("button", { name: "Continue with Passkey" }).click();

  // Authenticate to add the new passkey
  const auth2 = dummyAuth();
  auth2(page);
  await page.getByRole("button", { name: "Create Passkey" }).click();

  // Verify that we now have two passkeys
  await expect(page.getByText("Chrome")).toHaveCount(2);

  const passkeyName = "Secondary Passkey";
  await renamePasskey(page, passkeyName);

  // Verify the dialog closes and we now have both passkeys with different names
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).not.toBeVisible();
  await expect(page.getByText("Chrome")).toHaveCount(1);
  await expect(page.getByText(passkeyName)).toHaveCount(1);
});

test("User cannot rename passkey to an empty name nor is it renamed on cancel", async ({
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

  // Click the rename button for the passkey
  await page.getByLabel("Rename current Passkey").click();

  // Verify the rename dialog opens
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeVisible();

  const input = page.getByRole("textbox");
  const saveButton = page.getByRole("button", { name: "Save" });

  // Initially, the Save button should be enabled with the pre-filled name
  await expect(saveButton).toBeEnabled();

  // Clear the input field (make it empty)
  await input.clear();

  // Verify the Save button is disabled for empty input
  await expect(saveButton).toBeDisabled();

  // Try entering only whitespace
  await input.fill("   ");

  // Verify the Save button remains disabled for whitespace-only input
  await expect(saveButton).toBeDisabled();

  // Enter a valid name to verify Save becomes enabled again
  await input.clear();
  const passkeyName = "Valid Name";
  await input.fill(passkeyName);
  await expect(saveButton).toBeEnabled();

  // Cancel the dialog and verify no changes were made
  await page.getByRole("button", { name: "Cancel" }).click();

  // Verify the dialog closes and the original name is still there
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).not.toBeVisible();
  await expect(page.getByText("Chrome")).toHaveCount(1);
  await expect(page.getByText(passkeyName)).not.toBeVisible();
});
