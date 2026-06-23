import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { createActorForCredential, II_URL } from "../../utils";
import { DEFAULT_PASSKEY_NAME } from "../../fixtures/manageAccessPage";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
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
    setCredentialsForIdentity,
  }) => {
    const existingCredential = identities[0].credentials[0];
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage.add((dialog) => dialog.passkey());
    await manageAccessPage.assertPasskeyCount(2);
    const newCredential = identities[0].credentials.find(
      (credential) =>
        credential.credentialId !== existingCredential.credentialId,
    )!;
    expect(newCredential).toBeDefined();

    // Verify we can still sign in with the existing passkey
    await setCredentialsForIdentity(identities[0].identityNumber, [
      existingCredential,
    ]);
    await managePage.signOut();
    await manageAccessPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await manageAccessPage.assertVisible();

    // Verify we can now also sign in with the new passkey
    await managePage.signOut();
    await setCredentialsForIdentity(identities[0].identityNumber, [
      newCredential,
    ]);
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
    test.beforeEach(async ({ manageAccessPage }) => {
      // Rename passkey that's currently in use
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("in-use-passkey");
          await dialog.submit();
        });

      // Add additional passkey and rename it
      await manageAccessPage.add((dialog) => dialog.passkey());
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

    test("currently in use passkey is disabled when other methods exist", async ({
      manageAccessPage,
    }) => {
      await manageAccessPage.assertPasskeyCount(2);
      await manageAccessPage
        .findPasskey("in-use-passkey")
        .assertRemoveDisabledWithTooltip(
          "Switch to another method before removing",
        );
    });
  });

  test("cannot remove a single passkey without a recovery method", async ({
    manageAccessPage,
  }) => {
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage
      .findPasskey(DEFAULT_PASSKEY_NAME)
      .assertRemoveDisabledWithTooltip(
        "Add another method or sign in via recovery to remove",
      );
    await manageAccessPage.assertPasskeyCount(1);
  });

  test("switch is disabled when already signed in with that passkey", async ({
    manageAccessPage,
  }) => {
    await manageAccessPage.assertPasskeyCount(1);
    await manageAccessPage
      .findPasskey(DEFAULT_PASSKEY_NAME)
      .assertSwitchDisabled();
  });

  test("cannot have more than 16 passkeys", async ({
    page,
    managePage,
    manageAccessPage,
    identities,
    signInWithIdentity,
  }) => {
    // Verify we start with 1 passkey
    await manageAccessPage.assertPasskeyCount(1);
    await managePage.signOut();

    // Add 15 passkeys via an actor to avoid getting
    // rate limited by Chrome when adding via the UI.
    const actor = await createActorForCredential(
      identities[0].host,
      identities[0].canisterId,
      identities[0].credentials[0],
    );
    for (let i = 0; i < 15; i++) {
      const identity = await ECDSAKeyIdentity.generate();
      await actor.authn_method_add(identities[0].identityNumber, {
        metadata: [
          ["alias", { String: `passkey-${i}` }],
          ["origin", { String: II_URL }],
        ],
        authn_method: {
          WebAuthn: {
            pubkey: identity.getPublicKey().toDer(),
            credential_id: crypto.getRandomValues(new Uint8Array(16)),
            aaguid: [],
          },
        },
        security_settings: {
          protection: { Unprotected: null },
          purpose: { Authentication: null },
        },
        last_authentication: [],
      });
    }

    // Sign in and assert all 16 passkeys are present
    await manageAccessPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await manageAccessPage.assertPasskeyCount(16);

    // Assert we cannot add another passkey via the UI
    await manageAccessPage.add(async (dialog) => {
      await dialog.assertPasskeyUnavailable();
      await dialog.close();
    });
    await manageAccessPage.assertPasskeyCount(16);
  });

  test("returns to the chooser when the cross-device dialog is closed", async ({
    page,
  }) => {
    // Open the "Add access method" chooser.
    await page
      .getByRole("main")
      .getByRole("button", { name: "Add new" })
      .click();
    await expect(
      page.getByRole("heading", { name: "Add access method" }),
    ).toBeVisible();

    // Open the cross-device pairing modal via the single "URL | QR Code" link.
    await page.getByRole("button", { name: "URL | QR Code" }).click();
    await expect(
      page.getByRole("heading", {
        level: 1,
        name: "Add this identity to another device",
      }),
    ).toBeVisible();

    // The pairing modal is layered on top of the chooser; closing it returns
    // to the chooser rather than tearing down the whole add-access dialog.
    // Both <dialog>s match getByRole("dialog") (the pairing one is nested),
    // so target the innermost via .last().
    const pairingDialog = page
      .getByRole("dialog")
      .filter({
        has: page.getByRole("heading", {
          name: "Add this identity to another device",
        }),
      })
      .last();
    await pairingDialog.getByRole("button", { name: "Close" }).click();

    await expect(
      page.getByRole("heading", {
        level: 1,
        name: "Add this identity to another device",
      }),
    ).toBeHidden();
    // The chooser is still open — the dialog was not torn down.
    await expect(
      page.getByRole("heading", { name: "Add access method" }),
    ).toBeVisible();
    await expect(
      page.getByRole("button", { name: "Continue with passkey" }),
    ).toBeVisible();
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
        const actor = await createActorForCredential(
          identities[0].host,
          identities[0].canisterId,
          identities[0].credentials[0],
        );
        const identity = await ECDSAKeyIdentity.generate();
        await actor.authn_method_add(identities[0].identityNumber, {
          metadata: [
            ["alias", { String: LEGACY_PASSKEY_NAME }],
            ["origin", { String: LEGACY_II_URL }],
          ],
          authn_method: {
            WebAuthn: {
              pubkey: identity.getPublicKey().toDer(),
              credential_id: crypto.getRandomValues(new Uint8Array(16)),
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
        .assertRemoveDisabled();
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
      // Rename current so we can identify it after adding a second passkey
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("in-use-passkey");
          await dialog.submit();
        });

      // Add another passkey — the active method cannot be removed, so we
      // cancel removal on the non-active one instead
      await manageAccessPage.add((dialog) => dialog.passkey());
      await manageAccessPage
        .findPasskey(DEFAULT_PASSKEY_NAME)
        .rename(async (dialog) => {
          await dialog.fill("additional-passkey");
          await dialog.submit();
        });

      // Cancel removal of the non-active passkey
      await manageAccessPage
        .findPasskey("additional-passkey")
        .remove((dialog) => dialog.cancel());

      // Cleanup: remove the additional passkey and restore the original name
      await manageAccessPage
        .findPasskey("additional-passkey")
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
