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

    await expect(
      page.getByRole("heading", {
        level: 2,
        exact: true,
        name: "Access methods",
      }),
    ).toBeVisible();

    // Check that we have one passkey listed
    const passkey = await page.getByText("Chrome");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });
});
