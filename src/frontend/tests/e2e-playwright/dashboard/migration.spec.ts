import { expect, test } from "@playwright/test";
import { dummyAuth, II_URL } from "../utils";

const TEST_USER_NAME = "Test User";
const LEGACY_II_URL = "https://identity.ic0.app";

test.describe("Migration", () => {
  test("User can migrate a legacy identity", async ({ page, context }) => {
    // Step 1: Create a legacy identity
    await page.goto(LEGACY_II_URL);

    // Enable WebAuthn Virtual Authenticator
    const client = await context.newCDPSession(page);
    await client.send("WebAuthn.enable");
    await client.send("WebAuthn.addVirtualAuthenticator", {
      options: {
        protocol: "ctap2",
        transport: "usb",
        hasResidentKey: true,
        hasUserVerification: true,
        isUserVerified: true,
      },
    });

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
    await page.getByRole("button", { name: "Create Passkey" }).click();

    // Step 4: Verify the migration was successful
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
  });
});
