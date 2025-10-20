import { expect, test } from "@playwright/test";
import { authorize, createIdentity, dummyAuth } from "../utils";

test("Create another account and authorize with primary", async ({ page }) => {
  const auth = dummyAuth();
  const primaryPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Work account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage
      .getByRole("button", { name: "Continue with My Test Dapp account" })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
});

test("Create another account and authorize with it", async ({ page }) => {
  const auth = dummyAuth();
  const primaryPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Social account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage
      .getByRole("button", { name: "Continue with Social account" })
      .click();
  });
  expect(principal).not.toEqual(primaryPrincipal);
});

test("Create another account, make it default and authorize with it", async ({
  page,
}) => {
  const auth = dummyAuth();
  const primaryPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Test account");
    await authPage
      .getByRole("checkbox", { name: "Set as default sign-in" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage
      .getByRole("button", { name: "Continue with My Test Dapp account" })
      .click();
  });
  const secondaryPrincipal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
  expect(secondaryPrincipal).not.toEqual(primaryPrincipal);
});

test("Rename primary account and authorize with it", async ({ page }) => {
  const auth = dummyAuth();
  const primaryPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage
      .getByRole("button", { name: "Edit My Test Dapp account" })
      .click();
    await authPage.getByLabel("Account name").fill("Renamed account");
    await authPage.getByRole("button", { name: "Save changes" }).click();
    await authPage
      .getByRole("button", { name: "Continue with Renamed account" })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
});

test("Rename secondary account and authorize with it", async ({ page }) => {
  const auth = dummyAuth();
  const primaryPrincipal = await createIdentity(page, "John Doe", auth);
  const principal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Test account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage
      .getByRole("button", { name: "Continue with My Test Dapp account" })
      .click();
  });
  const secondaryPrincipal = await authorize(page, async (authPage) => {
    auth(authPage);
    await authPage
      .getByRole("switch", { name: "Enable multiple accounts" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Edit Test account" }).click();
    await authPage.getByLabel("Account name").fill("Renamed account");
    await authPage.getByRole("button", { name: "Save changes" }).click();
    await authPage
      .getByRole("button", { name: "Continue with Renamed account" })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
  expect(secondaryPrincipal).not.toEqual(primaryPrincipal);
});

// test("Can't create more than 5 accounts", async ({ page }) => {
//   const auth = dummyAuth();
//   await createIdentity(page, "John Doe", auth);
//   await authorize(page, async (authPage) => {
//     auth(authPage);
//     await authPage
//       .getByRole("button", { name: "Create or use another account" })
//       .click();
//     for (let i = 1; i <= 4; i++) {
//       await authPage
//         .getByRole("button", { name: "Create additional account" })
//         .click();
//       await authPage.getByLabel("Account name").fill(`Additional account ${i}`);
//       await authPage.getByRole("button", { name: "Create account" }).click();
//     }
//     await expect(
//       authPage.getByRole("button", { name: "Create additional account" }),
//     ).toBeDisabled();
//     // Authentication needs to complete in `authorize()` to pass the test
//     await authPage.getByRole("button", { name: "Primary account" }).click();
//   });
// });
