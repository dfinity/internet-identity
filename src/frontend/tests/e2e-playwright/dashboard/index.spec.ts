import { expect, test } from "@playwright/test";
import { clearStorage, createIdentity, dummyAuth, II_URL } from "../utils";

const TEST_USER_NAME = "Test User";

test.describe("Dashboard Navigation", () => {
  test("User can register, sign in, access the dashboard and navigate to security page", async ({
    page,
  }) => {
    const auth = dummyAuth();
    await createIdentity(page, TEST_USER_NAME, auth);
    await clearStorage(page);
    await page.goto(II_URL);
    await page.getByRole("link", { name: "Manage Identity" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use existing identity" }).click();

    // Verify we're at the dashboard
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();

    // Navigate to access methods
    const menuButton = page.getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
    await page.getByRole("link", { name: "Access methods" }).click();

    // Check that we have one passkey listed
    const passkey = await page.getByText("Chrome");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });
});
