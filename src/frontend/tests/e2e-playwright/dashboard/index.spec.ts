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
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();

    // Verify we're at the dashboard
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: TEST_USER_NAME }),
    ).toBeVisible();

    // Navigate to security page
    await page
      .getByRole("navigation")
      .getByRole("link", { name: "Security" })
      .click();

    await page.waitForURL(II_URL + "/manage/security");
    await expect(page.getByRole("heading", { name: "Security" })).toBeVisible();
    // Needed to make sure the navigation is done
    await expect(page.getByLabel("Go to Security")).not.toBeVisible();

    // Check that we have one passkey listed
    const passkey = await page.getByText("Chrome");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });
});
