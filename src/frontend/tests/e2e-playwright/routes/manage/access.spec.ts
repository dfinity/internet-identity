import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { dummyAuth, getRandomIndex, II_URL } from "../../utils";
import { DEFAULT_PASSKEY_NAME } from "../../fixtures/manageAccessPage";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { toSeed } from "../../fixtures/identity";
import { LEGACY_II_URL } from "$lib/config";

test.describe("Access methods", () => {
  test.beforeEach(
    async ({ page, manageAccessPage, identities, signInWithIdentity }) => {
      await manageAccessPage.goto();
      await signInWithIdentity(page, identities[0].identityNumber);
    },
  );

  test("can add a passkey", async ({
    page,
    managePage,
    manageAccessPage,
    identities,
    signInWithIdentity,
    replaceAuthForIdentity,
  }) => {
    const authIndex = getRandomIndex();
    const auth = dummyAuth(authIndex);
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.add((dialog) => dialog.passkey(auth));
    await manageAccessPage.assertPasskeyCount(2);

    // Verify we can still sign in with the existing passkey
    await managePage.signOut();
    await manageAccessPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await manageAccessPage.assertVisible();

    // Verify we can now also sign in with the new passkey
    await managePage.signOut();
    replaceAuthForIdentity(identities[0].identityNumber, authIndex);
    await manageAccessPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await manageAccessPage.assertVisible();
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
    test.beforeEach(async ({ manageAccessPage, identities }) => {
      // Rename passkey that's currently in use
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("in-use-passkey");
          await dialog.submit();
        });

      // Add additional passkey and rename it
      const auth = dummyAuth(identities[0].dummyAuthIndex + BigInt(1));
      await manageAccessPage.add((dialog) => dialog.passkey(auth));
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("additional-passkey");
          await dialog.submit();
        });
    });

    test("which is currently not in use", async ({ manageAccessPage }) => {
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
      page,
      manageAccessPage,
      identities,
      signInWithIdentity,
      replaceAuthForIdentity,
    }) => {
      // Remove currently in use passkey
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage
        .findPasskey("in-use-passkey")
        .remove(async (dialog) => {
          await dialog.assertSignOutWarningShown();
          await dialog.confirm();
        });

      await page.waitForURL(II_URL); // Expect to be signed out
      await manageAccessPage.goto(); // Go back to the manage page

      // Sign in with the new passkey and assert it's the only passkey
      replaceAuthForIdentity(
        identities[0].identityNumber,
        identities[0].dummyAuthIndex + BigInt(1),
      );
      await signInWithIdentity(page, identities[0].identityNumber);
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("additional-passkey");
    });
  });

  test("cannot remove a single passkey", async ({ manageAccessPage }) => {
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage
      .findPasskey(DEFAULT_PASSKEY_NAME)
      .assertUnremovable();
    await manageAccessPage.assertPasskeyCount(1);
  });

  test("cannot have more than 16 passkeys", async ({ manageAccessPage }) => {
    await manageAccessPage.assertPasskeyCount(1);
    for (let i = 0; i < 15; i++) {
      await manageAccessPage.add((dialog) => dialog.passkey(dummyAuth()));
    }
    await manageAccessPage.assertPasskeyCount(16);
    await manageAccessPage.add(async (dialog) => {
      await dialog.assertPasskeyUnavailable();
      await dialog.close();
    });
    await manageAccessPage.assertPasskeyCount(16);
  });

  test.describe("can remove a legacy passkey", () => {
    const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

    test.beforeEach(
      async ({
        page,
        managePage,
        manageAccessPage,
        identities,
        signInWithIdentity,
        actorForIdentity,
      }) => {
        // Rename current passkey so we can differentiate it from the legacy passkey
        await manageAccessPage.assertPasskeyCount(1);
        await manageAccessPage
          .findPasskey(DEFAULT_PASSKEY_NAME)
          .rename(async (dialog) => {
            await dialog.fill("post-upgrade-passkey");
            await dialog.submit();
          });

        // Use an actor to create a legacy passkey (not id.ai)
        // since this functionality is no longer available.
        const actor = await actorForIdentity(identities[0].identityNumber);
        const authIndex = getRandomIndex();
        const seed = toSeed(authIndex);
        const identity = await Ed25519KeyIdentity.generate(seed);
        await actor.authn_method_add(identities[0].identityNumber, {
          metadata: [
            ["alias", { String: LEGACY_PASSKEY_NAME }],
            ["origin", { String: LEGACY_II_URL }],
          ],
          authn_method: {
            WebAuthn: {
              pubkey: new Uint8Array(identity.getPublicKey().derKey),
              credential_id: seed,
              aaguid: [],
            },
          },
          security_settings: {
            protection: { Unprotected: null },
            purpose: { Authentication: null },
          },
          last_authentication: [],
        });

        // Sign back in and assert the legacy passkey is present and labeled as legacy
        await managePage.signOut();
        await manageAccessPage.goto();
        await signInWithIdentity(page, identities[0].identityNumber);
        await manageAccessPage.assertPasskeyCount(2);
        await manageAccessPage.assertPasskeyExists(LEGACY_PASSKEY_NAME);
        const passkeyItem = manageAccessPage.findPasskey(LEGACY_PASSKEY_NAME);
        await expect(
          passkeyItem.locator.getByText("Legacy", { exact: true }),
        ).toBeVisible();
      },
    );

    test("when recovery phrase is verified", async ({
      page,
      manageAccessPage,
      manageRecoveryPage,
      identities,
      signInWithIdentity,
    }) => {
      // Set-up recovery phrase and verify it to enable removal of legacy passkey
      await manageRecoveryPage.goto();
      await signInWithIdentity(page, identities[0].identityNumber);
      await manageRecoveryPage.activate(async (wizard) => {
        await wizard.acknowledge();
        const words = await wizard.writeDown();
        await wizard.verifySelecting(words);
      });

      // Go back to access methods page and assert we can now remove the legacy passkey
      await manageAccessPage.goto();
      await signInWithIdentity(page, identities[0].identityNumber);
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage
        .findPasskey(LEGACY_PASSKEY_NAME)
        .remove((dialog) => dialog.confirm());

      // Assert other passkeys are still present
      await manageAccessPage.assertPasskeyCount(1);
      await manageAccessPage.assertPasskeyExists("post-upgrade-passkey");
    });

    test("unless recovery phrase is missing or unverified", async ({
      manageAccessPage,
    }) => {
      // Assert remove button is disabled
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage
        .findPasskey(LEGACY_PASSKEY_NAME)
        .assertUnremovable();
    });
  });

  test.describe("can be cancelled", () => {
    test.afterEach(
      async ({
        page,
        managePage,
        manageAccessPage,
        identities,
        signInWithIdentity,
      }) => {
        // Verify the number of passkeys hasn't changed
        await manageAccessPage.assertPasskeyCount(1);

        // Verify we can still sign in with the existing passkey
        await managePage.signOut();
        await manageAccessPage.goto();
        await signInWithIdentity(page, identities[0].identityNumber);
        await manageAccessPage.assertVisible();
      },
    );

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
