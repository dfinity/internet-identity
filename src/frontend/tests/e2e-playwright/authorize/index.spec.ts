import { test, expect } from "@playwright/test";
import {
  authorize,
  authorizeWithUrl,
  clearStorage,
  createIdentity,
  dummyAuth,
  TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
} from "../utils";

test("Authorize by registering a new passkey", async ({ page }) => {
  const auth = dummyAuth();
  await authorize(page, async (authPage) => {
    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authPage
      .getByRole("button", { name: "Set up a new Passkey" })
      .click();
    await authPage.getByLabel("Identity name").fill("John Doe");
    auth(authPage);
    await authPage.getByRole("button", { name: "Create Passkey" }).click();
  });
});

test("Authorize by signing in with an existing passkey", async ({ page }) => {
  const auth = dummyAuth();
  await createIdentity(page, "John Doe", auth);
  await clearStorage(page);
  await authorize(page, async (authPage) => {
    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Use an existing Passkey" })
      .click();
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
});

test("App logo appears when app is known", async ({ page }) => {
  const auth = dummyAuth();
  await authorizeWithUrl(page, TEST_APP_URL, async (authPage) => {
    await expect(authPage.locator('img[alt*="logo"]')).toBeVisible();

    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authPage
      .getByRole("button", { name: "Set up a new Passkey" })
      .click();
    await authPage.getByLabel("Identity name").fill("John Doe");
    auth(authPage);
    await authPage.getByRole("button", { name: "Create Passkey" }).click();
  });
});

test("App logo doesn't appear when app is not known", async ({ page }) => {
  const auth = dummyAuth();
  await authorizeWithUrl(page, TEST_APP_CANONICAL_URL, async (authPage) => {
    await expect(authPage.locator('[aria-hidden="true"] svg')).toBeVisible();
    await expect(authPage.locator('img[alt*="logo"]')).not.toBeVisible();

    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authPage
      .getByRole("button", { name: "Set up a new Passkey" })
      .click();
    await authPage.getByLabel("Identity name").fill("John Doe");
    auth(authPage);
    await authPage.getByRole("button", { name: "Create Passkey" }).click();
  });
});
