import { test } from "../fixtures";
import { expect } from "@playwright/test";
import { generateMnemonic } from "$lib/utils/recoveryPhrase";
import { getRandomIndex, II_URL, LEGACY_II_URL } from "../utils";
import { toSeed } from "../fixtures/identity";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";

test.describe("Upgrade flow", () => {
  const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

  // Create legacy identity and navigate to upgrade page
  test.beforeEach(
    async ({
      page,
      manageRecoveryPage,
      recoveryPage,
      identities,
      signInWithIdentity,
      actorForIdentity,
      replaceAuthForIdentity,
    }) => {
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

      // Replace the auth in the fixture with the legacy passkey for the upgrade test
      replaceAuthForIdentity(identities[0].identityNumber, authIndex);

      // await manageRecoveryPage.goto();
      // await signInWithIdentity(page, identities[0].identityNumber);
      // await manageRecoveryPage.assertNotActivated();
      // words.current = await manageRecoveryPage.activate(async (wizard) => {
      //   await wizard.acknowledge();
      //   const recoveryPhrase = await wizard.writeDown();
      //   await wizard.verifySelecting(recoveryPhrase);
      //   return recoveryPhrase;
      // });
      // await manageRecoveryPage.assertActivated();
      // await recoveryPage.goto();
    },
  );

  test.describe("can upgrade identity", () => {
    // Verify we can sign in and see the dashboard, this indicates
    // that the upgrade was successful and the user is authenticated.
    test.afterEach(
      async ({ page, manageAccessPage, identities, signInWithIdentity }) => {
        await manageAccessPage.goto();
        await signInWithIdentity(page, identities[0].identityNumber);
        await manageAccessPage.assertVisible();
      },
    );

    test("on first attempt", async ({ recoveryPage, words, identities }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identities[0].name);
      });
    });

    test("on retry after identity not found", async ({
      recoveryPage,
      words,
      identities,
    }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(generateMnemonic());
        await wizard.retryIdentityNotFound();
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identities[0].name);
      });
    });

    test("on retry after invalid phrase", async ({
      recoveryPage,
      words,
      identities,
    }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(changeLastWord(words.current!));
        await wizard.retryInvalid();
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identities[0].name);
      });
    });

    test("with a legacy identity", async ({
      recoveryPage,
      words,
      identities,
      actorForIdentity,
    }) => {
      // Remove name from identity
      const actor = await actorForIdentity(identities[0].identityNumber);
      await actor.identity_properties_replace(identities[0].identityNumber, {
        name: [],
      });
      // Enter name during recovery
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.enterMissingName("Upgraded test user");
      });
    });
  });

  test("can be completed more than once", async ({
    page,
    recoveryPage,
    words,
    identities,
  }) => {
    // Recover with the same phrase twice to verify that
    // the phrase remains usable after it has been used.
    for (let i = 0; i < 2; i++) {
      await recoveryPage.goto();
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identities[0].name);
      });
      // Verify we've reached the dashboard and the page has loaded,
      // this indicates that the user has successfully authenticated.
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    }
  });

  test.describe("can be cancelled", () => {
    // Assert we're back on the landing page and the recovery phrase is still valid
    test.afterEach(async ({ page, recoveryPage, words, identities }) => {
      await page.waitForURL(II_URL);
      await recoveryPage.goto();
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.confirmFoundIdentity(identities[0].name);
      });
      await page.waitForURL(II_URL + "/manage/access");
      await expect(
        page.getByRole("heading", { name: "Access methods" }),
      ).toBeVisible();
    });

    test("before starting", async ({ recoveryPage }) => {
      await recoveryPage.cancel();
    });

    test("before submission", async ({ recoveryPage }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.close();
      });
      await recoveryPage.cancel();
    });

    test("after found identity", async ({ recoveryPage, words }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.cancelFoundIdentity();
      });
      await recoveryPage.cancel();
    });

    test("after identity not found", async ({ recoveryPage }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(generateMnemonic());
        await wizard.cancelIdentityNotFound();
      });
      await recoveryPage.cancel();
    });

    test("after invalid phrase", async ({ recoveryPage, words }) => {
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(changeLastWord(words.current!));
        await wizard.cancelInvalid();
      });
      await recoveryPage.cancel();
    });

    test("before upgrading legacy identity", async ({
      recoveryPage,
      words,
      identities,
      actorForIdentity,
    }) => {
      // Remove name from identity
      const actor = await actorForIdentity(identities[0].identityNumber);
      await actor.identity_properties_replace(identities[0].identityNumber, {
        name: [],
      });
      // Cancel instead of entering an identity name
      await recoveryPage.start(async (wizard) => {
        await wizard.enterRecoveryPhrase(words.current!);
        await wizard.cancelMissingName();
      });
      await recoveryPage.cancel();
      // Revert name removal from identity
      await actor.identity_properties_replace(identities[0].identityNumber, {
        name: [identities[0].name],
      });
    });
  });
});
