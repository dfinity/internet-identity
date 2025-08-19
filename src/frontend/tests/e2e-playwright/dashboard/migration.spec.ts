import { expect, test } from "@playwright/test";
import {
  addVirtualAuthenticator,
  clearStorage,
  II_URL,
  LEGACY_II_URL,
  signOut,
} from "../utils";

const TEST_USER_NAME = "Test User";

test.describe("Migration", () => {
  test("User can migrate a legacy identity", async ({ page }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    await addVirtualAuthenticator(page);

    await page
      .getByRole("button", { name: "Create Internet Identity" })
      .click();
    // Needs more time to load.
    await expect(page.locator("#userNumber")).toBeVisible({ timeout: 15_000 });
    const identityNumber = await page.locator("#userNumber").innerText();
    await page.getByRole("button", { name: "I saved it, continue" }).click();

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);

    // Step 3: Perform the migration
    await page
      .getByRole("button", { name: "Upgrade from legacy identity" })
      .click();
    await page
      .getByPlaceholder("Internet Identity number")
      .fill(identityNumber);
    await page.getByRole("button", { name: "Continue" }).click();

    await page.getByLabel("Identity name").fill(TEST_USER_NAME);
    // const auth = dummyAuth();
    // auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();

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
    // auth(page);
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

    await page
      .getByRole("button", { name: "Create Internet Identity" })
      .click();
    // Needs more time to load.
    await expect(page.locator("#userNumber")).toBeVisible({ timeout: 15_000 });
    const identityNumber = await page.locator("#userNumber").innerText();
    await page.getByRole("button", { name: "I saved it, continue" }).click();

    // Step 2: Navigate to the new II_URL to start the migration
    await page.goto(II_URL);

    // Step 3: Perform the migration
    await page
      .getByRole("button", { name: "Upgrade from legacy identity" })
      .click();
    await page
      .getByPlaceholder("Internet Identity number")
      .fill(identityNumber);
    await page.getByRole("button", { name: "Continue" }).click();

    await page.getByLabel("Identity name").fill(TEST_USER_NAME);
    // const auth = dummyAuth();
    // auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();

    // Step 4: Verify the migration was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();

    await signOut(page);
    await clearStorage(page);

    // Step 5: Login again
    await page.goto(II_URL);
    // auth(page);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
  });
});
