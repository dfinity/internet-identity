import { test } from "../fixtures";
import { dummyAuth, getRandomIndex, II_URL } from "../utils";
import { DEFAULT_PASSKEY_NAME } from "../fixtures/manageAccessPage";

test.describe("Access methods", () => {
  test.beforeEach(async ({ identity, manageAccessPage }) => {
    await manageAccessPage.goto();
    await identity.signIn();
  });

  test("can add a passkey", async ({ manageAccessPage, identity }) => {
    const authIndex = getRandomIndex();
    const auth = dummyAuth(authIndex);
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.add((wizard) => wizard.passkey(auth));
    await manageAccessPage.assertPasskeyCount(2);
    // Verify we can sign in with the new passkey
    await identity.signOut();
    identity.replaceAuth(authIndex);
    await manageAccessPage.goto();
    await identity.signIn();
    await manageAccessPage.assertPasskeyCount(2);
  });

  test("can rename a passkey", async ({ manageAccessPage }) => {
    // Passkeys are tied to a particular device in e.g. Windows Hello,
    // so below is an example where it's renamed to know which device.
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.findPasskey(DEFAULT_PASSKEY_NAME).rename("Dell XPS");
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.assertPasskeyExists("Dell XPS");
  });

  test.describe("can remove a passkey", () => {
    test("which has been added", async ({ manageAccessPage }) => {
      // Rename passkey that's currently in use
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename("in-use-passkey");

      // Add additional passkey and rename it
      const auth = dummyAuth();
      await manageAccessPage.add((wizard) => wizard.passkey(auth));
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename("additional-passkey");

      // Remove additional passkey
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage.findPasskey("additional-passkey").remove(false);

      // Assert it has been removed
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("in-use-passkey");
    });

    test("that's currently in use", async ({
      manageAccessPage,
      identity,
      page,
    }) => {
      // Rename passkey that's currently in use
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename("in-use-passkey");

      // Add additional passkey and rename it
      const authIndex = getRandomIndex();
      const auth = dummyAuth(authIndex);
      await manageAccessPage.add((wizard) => wizard.passkey(auth));
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename("additional-passkey");

      // Remove currently in use passkey
      await manageAccessPage.findPasskey("in-use-passkey").remove(true);

      // Assert it has been removed
      await page.waitForURL(II_URL); // Expect to be signed out
      await manageAccessPage.goto(); // Go back to the manage page
      identity.replaceAuth(authIndex); // Sign in with new passkey
      await identity.signIn();
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("additional-passkey");
    });
  });

  test("cannot remove a single passkey", async ({ manageAccessPage }) => {
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage
      .findPasskey(DEFAULT_PASSKEY_NAME)
      .assertUnremovable();
  });

  test("cannot have more than 8 passkeys", async ({ manageAccessPage }) => {
    await manageAccessPage.assertPasskeyCount(1);
    for (let i = 0; i < 7; i++) {
      await manageAccessPage.add((wizard) => wizard.passkey(dummyAuth()));
    }
    await manageAccessPage.assertPasskeyCount(8);
    await manageAccessPage.add(async (wizard) => {
      await wizard.assertPasskeyUnavailable();
      await wizard.close();
    });
  });
});
