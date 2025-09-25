import { expect, test } from "../fixtures";
import {
  authorize,
  authorizeWithUrl,
  createIdentity,
  dummyAuth,
  TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  II_URL,
  createNewIdentityInII,
} from "../utils";

test("Authorize with last used identity", async ({ page }) => {
  const auth = dummyAuth();
  const expectedPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
});

test("Authorize by switching to another identity", async ({ page }) => {
  const auth1 = dummyAuth();
  const auth2 = dummyAuth();
  const expectedPrincipal = await createIdentity(page, "John Doe", auth1);
  const otherPrincipal = await createIdentity(page, "Jane Doe", auth2);
  const principal = await authorize(page, async (authPage) => {
    await authPage.getByRole("button", { name: "Switch identity" }).click();
    await authPage.getByRole("button", { name: "John Doe" }).click();
    auth1(authPage);
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
  expect(principal).not.toBe(otherPrincipal);
});

test("Authorize by creating a new identity", async ({ page }) => {
  const auth1 = dummyAuth();
  const auth2 = dummyAuth();
  const initialPrincipal = await createIdentity(page, "John Doe", auth1);
  const newIdentityPrincipal = await authorize(page, async (authPage) => {
    await createNewIdentityInII(authPage, "Jane Doe", auth2);
  });
  expect(newIdentityPrincipal).not.toBe(initialPrincipal);
});

test("Authorize by signing in with a different passkey", async ({ page }) => {
  const auth1 = dummyAuth();
  const auth2 = dummyAuth();
  const expectedPrincipal = await createIdentity(page, "John Doe", auth1);
  const otherPrincipal = await createIdentity(page, "Jane Doe", auth2);
  const principal = await authorize(page, async (authPage) => {
    await authPage.getByRole("button", { name: "Switch identity" }).click();
    await authPage
      .getByRole("button", { name: "Use another identity" })
      .click();
    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    auth1(authPage);
    await authPage
      .getByRole("button", { name: "Use an existing Passkey" })
      .click();
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
  expect(principal).not.toBe(otherPrincipal);
});

test("App logo appears when app is known", async ({ page }) => {
  const auth = dummyAuth();
  await createIdentity(page, "John Doe", auth);
  await authorizeWithUrl(page, TEST_APP_URL, II_URL, async (authPage) => {
    await expect(authPage.locator('img[alt*="logo"]')).toBeVisible();

    auth(authPage);
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
});

test("App logo doesn't appear when app is not known", async ({ page }) => {
  const auth = dummyAuth();
  await createIdentity(page, "John Doe", auth);
  await authorizeWithUrl(
    page,
    TEST_APP_CANONICAL_URL,
    II_URL,
    async (authPage) => {
      await expect(authPage.locator('[aria-hidden="true"] svg')).toBeVisible();
      await expect(authPage.locator('img[alt*="logo"]')).not.toBeVisible();

      auth(authPage);
      await authPage.getByRole("button", { name: "Primary account" }).click();
    },
  );
});
