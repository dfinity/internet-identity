import { test } from "../fixtures";
import { dummyAuth, getRandomIndex, II_URL } from "../utils";
import { DEFAULT_PASSKEY_NAME } from "../fixtures/manageAccessPage";
import { Identity } from "../fixtures/identity";
import { expect } from "@playwright/test";

test.describe("Access methods", () => {
  test.beforeEach(async ({ identity, manageAccessPage }) => {
    await manageAccessPage.goto();
    await identity.signIn();
  });

  test("can add a passkey", async ({ manageAccessPage, identity }) => {
    const authIndex = getRandomIndex();
    const auth = dummyAuth(authIndex);
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.add((dialog) => dialog.passkey(auth));
    await manageAccessPage.assertPasskeyCount(2);

    // Verify we can still sign in with the existing passkey
    await identity.signOut();
    await manageAccessPage.goto();
    await identity.signIn();

    // Verify we can now also sign in with the new passkey
    await identity.signOut();
    identity.replaceAuth(authIndex);
    await manageAccessPage.goto();
    await identity.signIn();
  });

  test.describe("can rename a passkey", () => {
    test("to device name", async ({ manageAccessPage }) => {
      // Passkeys are tied to a particular device in e.g. Windows Hello,
      // so this is an example where it's renamed to know which device.
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("Dell XPS");
          await dialog.submit();
        });
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("Dell XPS");
    });

    test("unless it's an empty string", async ({ manageAccessPage }) => {
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("");
          await dialog.assertSubmitDisabled();
          await dialog.close();
        });
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists(DEFAULT_PASSKEY_NAME);
    });
  });

  test.describe("can remove a passkey", () => {
    // Use next index for the additional passkey
    const additionalIndex = (identity: Identity) =>
      identity.authIndex + BigInt(1);

    test.beforeEach(async ({ manageAccessPage, identity }) => {
      // Rename passkey that's currently in use
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("in-use-passkey");
          await dialog.submit();
        });

      // Add additional passkey and rename it
      const auth = dummyAuth(additionalIndex(identity));
      await manageAccessPage.add((dialog) => dialog.passkey(auth));
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("additional-passkey");
          await dialog.submit();
        });
    });

    test("which has been added previously", async ({ manageAccessPage }) => {
      // Remove additional passkey
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage
        .findPasskey("additional-passkey")
        .remove((dialog) => dialog.confirm());

      // Assert it has been removed
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("in-use-passkey");
    });

    test("which is currently in use", async ({
      manageAccessPage,
      identity,
      page,
    }) => {
      // Remove currently in use passkey
      await manageAccessPage
        .findPasskey("in-use-passkey")
        .remove(async (dialog) => {
          await dialog.assertSignOutWarningShown();
          await dialog.confirm();
        });

      await page.waitForURL(II_URL); // Expect to be signed out
      await manageAccessPage.goto(); // Go back to the manage page

      // Sign in with the new passkey and assert it's the only passkey
      identity.replaceAuth(additionalIndex(identity));
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
      await manageAccessPage.add((dialog) => dialog.passkey(dummyAuth()));
    }
    await manageAccessPage.assertPasskeyCount(8);
    await manageAccessPage.add(async (dialog) => {
      await dialog.assertPasskeyUnavailable();
      await dialog.close();
    });
  });

  test.describe("can be cancelled", () => {
    test.afterEach(async ({ identity, manageAccessPage }) => {
      // Verify the number of passkeys hasn't changed
      await manageAccessPage.assertPasskeyCount(1);

      // Verify we can still sign in with the existing passkey
      await identity.signOut();
      await manageAccessPage.goto();
      await identity.signIn();
    });

    test("when adding an access method", async ({ manageAccessPage }) => {
      await manageAccessPage.add((dialog) => dialog.close());
    });

    test("when renaming a passkey", async ({ manageAccessPage }) => {
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename((dialog) => dialog.close());
    });

    test("when removing a passkey", async ({ manageAccessPage }) => {
      // Rename current
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("in-use-passkey");
          await dialog.submit();
        });

      // Add another, since we cannot remove a single passkey
      await manageAccessPage.add((dialog) => dialog.passkey(dummyAuth()));

      // Then remove the current passkey, but cancel instead of confirm
      await manageAccessPage
        .findPasskey("in-use-passkey")
        .remove((dialog) => dialog.cancel());

      // Cleanup additional passkey and undo renaming of current
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .remove((dialog) => dialog.confirm());
      await manageAccessPage
        .findPasskey("in-use-passkey")
        .rename(async (dialog) => {
          await dialog.fill(DEFAULT_PASSKEY_NAME);
          await dialog.submit();
        });
    });
  });
});
