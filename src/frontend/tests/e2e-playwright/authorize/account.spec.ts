import { expect, test } from "@playwright/test";
import { authorize, createIdentity, dummyAuth } from "../utils";

test("Create and authorize with additional account", async ({ page }) => {
  const auth = dummyAuth();
  const otherPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Create or use another account" })
      .click();
    await authPage
      .getByRole("button", { name: "Create additional account" })
      .click();
    await authPage.getByLabel("Account name").fill("Work account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("button", { name: "Work account" }).click();
  });
  expect(principal).not.toBe(otherPrincipal);
});

test("Create additional account but authorize with default account", async ({
  page,
}) => {
  const auth = dummyAuth();
  const expectedPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Create or use another account" })
      .click();
    await authPage
      .getByRole("button", { name: "Create additional account" })
      .click();
    await authPage.getByLabel("Account name").fill("Social account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
});

test("Create and authorize with additional account, then switch back to primary account", async ({
  page,
}) => {
  const auth = dummyAuth();
  const expectedPrincipal = await createIdentity(page, "John Doe", auth);
  const otherPrincipal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Create or use another account" })
      .click();
    await authPage
      .getByRole("button", { name: "Create additional account" })
      .click();
    await authPage.getByLabel("Account name").fill("Private account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("button", { name: "Private account" }).click();
  });
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
  expect(principal).not.toBe(otherPrincipal);
});

test("Can't create more than 5 accounts", async ({ page }) => {
  const auth = dummyAuth();
  await createIdentity(page, "John Doe", auth);
  await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Create or use another account" })
      .click();
    for (let i = 1; i <= 4; i++) {
      await authPage
        .getByRole("button", { name: "Create additional account" })
        .click();
      await authPage.getByLabel("Account name").fill(`Additional account ${i}`);
      await authPage.getByRole("button", { name: "Create account" }).click();
    }
    await expect(
      authPage.getByRole("button", { name: "Create additional account" }),
    ).toBeDisabled();
    // Authentication needs to complete in `authorize()` to pass the test
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
});
