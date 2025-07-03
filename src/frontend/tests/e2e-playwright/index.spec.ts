import { expect, test } from "@playwright/test";
import { clearStorage, createIdentity, dummyAuth, II_URL } from "./utils";

const DEFAULT_USER_NAME = "John Doe";
const SECONDARY_USER_NAME = "Jane Doe";

test.describe("First visit", () => {
  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign in with an existing passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await clearStorage(page);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });
});

test.describe("Last used identities listed", () => {
  test("Sign in with last used identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    auth(page);
    await page.getByRole("button", { name: DEFAULT_USER_NAME }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign in with another identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill(SECONDARY_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${SECONDARY_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: SECONDARY_USER_NAME }),
    ).toBeVisible();
  });
});
