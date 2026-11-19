import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorize, II_URL } from "../../utils";

test("Create another account and authorize with primary", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Work account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "My Test Dapp account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
});

test("Create another account and authorize with it", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Social account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "Social account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).not.toEqual(primaryPrincipal);
});

test("Create another account, make it default and authorize with it", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Test account");
    await authPage
      .getByRole("checkbox", { name: "Set as default sign-in" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "My Test Dapp account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const secondaryPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    // With all options off, Continue signs in with the default account,
    // which is now "Test account".
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(false);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
  expect(secondaryPrincipal).not.toEqual(primaryPrincipal);
});

test("Make an existing account the default and authorize with it", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const workPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Work");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "Work" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const defaultPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    // The accounts are read with the stored session; saving signs in.
    await authPage.getByRole("button", { name: "Edit Work" }).click();
    await authPage
      .getByRole("checkbox", { name: "Set as default sign-in" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Save changes" }).click();
    await expect(authPage.getByRole("dialog")).toBeHidden();
    // With all options off, Continue signs in with the default account.
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(false);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(workPrincipal).not.toEqual(primaryPrincipal);
  expect(defaultPrincipal).toEqual(workPrincipal);
});

test("Rename primary account and authorize with it", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage
      .getByRole("button", { name: "Edit My Test Dapp account" })
      .click();
    await authPage.getByLabel("Account name").fill("Renamed account");
    await authPage.getByRole("button", { name: "Save changes" }).click();
    await authPage.getByRole("radio", { name: "Renamed account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
});

test("Rename secondary account and authorize with it", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Test account");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "My Test Dapp account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const secondaryPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Edit Test account" }).click();
    await authPage.getByLabel("Account name").fill("Renamed account");
    await authPage.getByRole("button", { name: "Save changes" }).click();
    await authPage.getByRole("radio", { name: "Renamed account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toEqual(primaryPrincipal);
  expect(secondaryPrincipal).not.toEqual(primaryPrincipal);
});

test("Can't create more than 5 accounts", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    for (let i = 1; i <= 4; i++) {
      await authPage
        .getByRole("button", { name: "Add another account" })
        .click();
      await authPage.getByLabel("Account name").fill(`Additional account ${i}`);
      await authPage.getByRole("button", { name: "Create account" }).click();
    }
    await expect(
      authPage.getByRole("button", { name: "Add another account" }),
    ).toBeDisabled();
    // Authentication needs to complete in `authorize()` to pass the test
    await authPage.getByRole("radio", { name: "My Test Dapp account" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
});

test("All options start from the account and duration last chosen on this browser", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  const primaryPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const workPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Work");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "Work" }).click();
    await authPage.getByRole("button", { name: "30 days" }).click();
    await authPage.getByRole("menuitem", { name: "1 day" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const rememberedPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await expect(authPage.getByRole("radio", { name: "Work" })).toBeChecked();
    await expect(authPage.getByRole("button", { name: "1 day" })).toBeVisible();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const optionsOffPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(false);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(workPrincipal).not.toEqual(primaryPrincipal);
  expect(rememberedPrincipal).toEqual(workPrincipal);
  expect(optionsOffPrincipal).toEqual(primaryPrincipal);
});

test("All options left on go off when there is no session to show them with, and the switch signs in", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const workPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("switch", { name: "Show all options" })
      .setChecked(true);
    await authPage.getByRole("button", { name: "Add another account" }).click();
    await authPage.getByLabel("Account name").fill("Work");
    await authPage.getByRole("button", { name: "Create account" }).click();
    await authPage.getByRole("radio", { name: "Work" }).click();
    await authPage.getByRole("button", { name: "30 days" }).click();
    await authPage.getByRole("menuitem", { name: "1 day" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });

  // The session Internet Identity kept on this browser is gone, as it is after
  // a month without a sign-in. The choice to see all options is not.
  const iiPage = await page.context().newPage();
  await iiPage.goto(II_URL);
  await iiPage.evaluate(
    () =>
      new Promise<void>((resolve, reject) => {
        const request = indexedDB.open("ii-session-delegations");
        request.onerror = () => reject(request.error);
        request.onsuccess = () => {
          const database = request.result;
          if (!database.objectStoreNames.contains("keys")) {
            database.close();
            resolve();
            return;
          }
          const transaction = database.transaction("keys", "readwrite");
          transaction.objectStore("keys").clear();
          transaction.oncomplete = () => {
            database.close();
            resolve();
          };
          transaction.onerror = () => reject(transaction.error);
        };
      }),
  );
  await iiPage.close();

  const switchedPrincipal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    const optionsSwitch = authPage.getByRole("switch", {
      name: "Show all options",
    });
    // Nothing signs in by itself: the options go off and Continue is ready.
    await expect(optionsSwitch).not.toBeChecked();
    await expect(optionsSwitch).toBeEnabled();
    await expect(authPage.getByRole("radio")).toHaveCount(0);
    // The switch signs in and shows the options as they were left.
    await optionsSwitch.setChecked(true);
    await expect(authPage.getByRole("radio", { name: "Work" })).toBeChecked({
      timeout: 15_000,
    });
    await expect(authPage.getByRole("button", { name: "1 day" })).toBeVisible();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(switchedPrincipal).toEqual(workPrincipal);
});
