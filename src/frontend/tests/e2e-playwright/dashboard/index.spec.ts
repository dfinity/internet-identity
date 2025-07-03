import { expect, test } from "@playwright/test";
import { clearStorage, createIdentity, dummyAuth, II_URL } from "../utils";

// Test user name
const TEST_USER_NAME = "Test User";

test.describe("Dashboard Navigation", () => {
  test("User can register, sign in, access the dashboard and navigate to security page", async ({
    page,
  }) => {
    // Create a test identity
    const auth = dummyAuth();
    await createIdentity(page, TEST_USER_NAME, auth);
    await clearStorage(page);

    // Sign in with the created identity
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();

    // Verify we're at the dashboard
    await page.waitForURL(II_URL + "/manage");
    await page
      .getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      })
      .isVisible();
    await page
      .getByRole("heading", { level: 5, name: TEST_USER_NAME })
      .isVisible();

    // Navigate to security page
    await page
      .getByRole("navigation")
      .getByRole("link", { name: "Security" })
      .click();

    // Verify we're at the security page
    await page.waitForURL(II_URL + "/manage/security");

    // Check that we have one passkey listed
    const passkey = await page.getByText("Passkey");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });
});
