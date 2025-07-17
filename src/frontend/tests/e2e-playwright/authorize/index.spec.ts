import { test } from "@playwright/test";
import { authorize, clearStorage, createIdentity, dummyAuth } from "../utils";

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
