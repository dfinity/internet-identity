import { expect, Page, test } from "@playwright/test";
import {
  addVirtualAuthenticator,
  clearStorage,
  dummyAuth,
  DummyAuthFn,
  II_URL,
  LEGACY_II_URL,
  signOut,
} from "../utils";

const TEST_USER_NAME = "Test User";
const TEST_USER_NAME_2 = "Test User 2";

const createLegacyIdentity = async (page: Page) => {
  await page.getByRole("button", { name: "Create Internet Identity" }).click();
  // Needs more time to load.
  await expect(page.locator("#userNumber")).toBeVisible({ timeout: 15_000 });
  const identityNumber = await page.locator("#userNumber").innerText();
  await page.getByRole("button", { name: "I saved it, continue" }).click();

  return identityNumber;
};

const upgradeLegacyIdentity = async (
  page: Page,
  identityNumber: string,
  auth: DummyAuthFn,
) => {
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  const dialog = page.getByRole("dialog");
  await expect(dialog).toBeVisible();
  await dialog.getByRole("button", { name: "Upgrade" }).click();
  await dialog
    .getByPlaceholder("Internet Identity number")
    .fill(identityNumber);
  await dialog.getByRole("button", { name: "Continue" }).click();

  await dialog.getByLabel("Identity name").fill(TEST_USER_NAME);
  auth(page);
  await dialog.getByRole("button", { name: "Upgrade identity" }).click();
};

test.describe("Migration", () => {
  test("User can migrate a legacy identity", async ({ page }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    await addVirtualAuthenticator(page);
    const identityNumber = await createLegacyIdentity(page);

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();

    // Step 3: Perform the migration
    const auth = dummyAuth();
    await upgradeLegacyIdentity(page, identityNumber, auth);

    // Step 4: Verify the migration was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();

    await signOut(page);

    // Step 5: Login again
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Switch identity" }).click();
    auth(page);
    await page.getByRole("button", { name: TEST_USER_NAME }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("User can migrate a legacy identity and use it as discoverable passkey", async ({
    page,
  }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    await addVirtualAuthenticator(page);
    const identityNumber = await createLegacyIdentity(page);

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();

    // Step 3: Perform the migration
    const auth = dummyAuth();
    await upgradeLegacyIdentity(page, identityNumber, auth);

    // Step 4: Verify the migration was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();

    await signOut(page);
    await clearStorage(page);

    // Step 5: Login again with discoverable passkey
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use existing identity" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("User can go through the upgrade flow again", async ({ page }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    await addVirtualAuthenticator(page);
    const identityNumber = await createLegacyIdentity(page);

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();

    // Step 3: Perform the first migration
    const auth = dummyAuth();
    await upgradeLegacyIdentity(page, identityNumber, auth);

    // Step 4: Verify the migration was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();

    // Step 5: Sign out
    await signOut(page);

    // Step 6: Navigate back to upgrade flow with the same identity number
    await page.goto(II_URL);
    const auth2 = dummyAuth();

    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    await page.getByRole("button", { name: "Upgrade" }).click();
    await page
      .getByPlaceholder("Internet Identity number")
      .fill(identityNumber);
    await page.getByRole("button", { name: "Continue" }).click();

    // Step 7: Verify "Identity already upgraded" screen appears
    await expect(
      page.getByRole("heading", { name: "Identity already upgraded" }),
    ).toBeVisible();
    await expect(
      page.getByText(
        "This identity has already been upgraded to the new experience",
      ),
    ).toBeVisible();

    // Step 8: Click "Upgrade again" button
    await page.getByRole("button", { name: "Upgrade again" }).click();

    // Step 9: Complete the upgrade flow again with a different name
    await page.getByLabel("Identity name").fill(TEST_USER_NAME_2);
    auth2(page);
    await page.getByRole("button", { name: "Upgrade identity" }).click();

    // Step 10: Verify the second upgrade was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME_2}!`),
      }),
    ).toBeVisible();
  });
});
