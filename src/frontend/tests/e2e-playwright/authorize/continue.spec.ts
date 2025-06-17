import { expect, test } from "@playwright/test";
import { authorize, createIdentity, dummyAuth } from "../utils";

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
