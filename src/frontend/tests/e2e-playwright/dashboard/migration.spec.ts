import { expect, test } from "../fixtures";
import type { Page } from "@playwright/test";
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
  await page.getByRole("button", { name: "Upgrade" }).click();
  await page.getByPlaceholder("Internet Identity number").fill(identityNumber);
  await page.getByRole("button", { name: "Continue" }).click();

  await page.getByLabel("Identity name").fill(TEST_USER_NAME);
  auth(page);
  await page.getByRole("button", { name: "Create Passkey" }).click();
};

test.describe("Migration", () => {
  test.skip(
    ({ browserName }) => browserName === "webkit",
    "Migration test not supported on Safari because it uses virtual authenticators which are not supported.",
  );

  test("User can migrate a legacy identity", async ({ page }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    await addVirtualAuthenticator(page);
    const identityNumber = await createLegacyIdentity(page);

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);
    await page.getByRole("link", { name: "Manage Identity" }).click();

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
    await page.getByRole("link", { name: "Manage Identity" }).click();
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
    await page.getByRole("link", { name: "Manage Identity" }).click();

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
    await page.getByRole("link", { name: "Manage Identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
  });
});
