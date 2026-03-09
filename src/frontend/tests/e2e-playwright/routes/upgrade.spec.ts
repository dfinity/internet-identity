import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  dummyAuth,
  getRandomIndex,
  II_URL,
  LEGACY_II_URL,
} from "../utils";
import { toSeed } from "../fixtures/identity";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";

test.describe("Upgrade flow", () => {
  const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

  // Create legacy identity and navigate to upgrade page
  test.beforeEach(
    async ({ page, identities, actorForIdentity, replaceAuthForIdentity }) => {
      // Navigate to legacy page that doesn't redirect
      await page.goto(LEGACY_II_URL + "/self-service");

      // Add virtual authenticator to be able to create a legacy passkey using navigator.credentials.create in the page context
      await addVirtualAuthenticator(page);

      // Create a passkey in the page context and return credential ID + DER public key bytes
      const { credentialId, publicKeyDer } = await page.evaluate(async () => {
        const credential = (await navigator.credentials.create({
          publicKey: {
            challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
            attestation: "direct",
            pubKeyCredParams: [
              { type: "public-key", alg: -7 },
              { type: "public-key", alg: -8 },
              { type: "public-key", alg: -257 },
            ],
            rp: {
              name: "Internet Identity Service",
            },
            user: {
              id: window.crypto.getRandomValues(new Uint8Array(16)),
              displayName: "Internet Identity User",
              name: "Internet Identity User",
            },
          },
        })) as PublicKeyCredential;

        const response =
          credential.response as AuthenticatorAttestationResponse;
        const publicKeyBuffer = response.getPublicKey();
        if (publicKeyBuffer === null) {
          throw new Error("Passkey attestation did not include a public key");
        }

        return {
          credentialId: Array.from(new Uint8Array(credential.rawId)),
          publicKeyDer: Array.from(new Uint8Array(publicKeyBuffer)),
        };
      });

      // Use an actor to create a legacy passkey (not id.ai)
      // since this functionality is no longer available.
      const actor = await actorForIdentity(identities[0].identityNumber);

      // Add the legacy passkey to the identity
      await actor.authn_method_add(identities[0].identityNumber, {
        metadata: [
          ["alias", { String: LEGACY_PASSKEY_NAME }],
          ["origin", { String: LEGACY_II_URL }],
        ],
        authn_method: {
          WebAuthn: {
            pubkey: new Uint8Array(publicKeyDer),
            credential_id: new Uint8Array(credentialId),
            aaguid: [],
          },
        },
        security_settings: {
          protection: { Unprotected: null },
          purpose: { Authentication: null },
        },
        last_authentication: [],
      });

      // Remove the dummy auth so the identity is only accessible via the legacy passkey,
      // which simulates the state of an identity that hasn't been upgraded yet.
      await actor.authn_method_remove(
        identities[0].identityNumber,
        new Uint8Array(
          Ed25519KeyIdentity.generate(toSeed(identities[0].dummyAuthIndex))
            .getPublicKey()
            .toDer(),
        ),
      );

      // Generate a new dummy auth index for identity
      replaceAuthForIdentity(identities[0].identityNumber, getRandomIndex());

      // Navigate to new II_URL
      await page.goto(II_URL);

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

  test("can upgrade identity", async ({ page, identities, managePage }) => {
    const dialog = page.getByRole("dialog");
    await page.getByRole("button", { name: "Sign in" }).click();
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Continue with passkey" }).click();
    await dialog.getByRole("button", { name: "Upgrade" }).click();
    await dialog
      .getByPlaceholder("Internet Identity number")
      .fill(identities[0].identityNumber.toString());
    await dialog.getByRole("button", { name: "Continue" }).click();
    await dialog.getByLabel("Identity name").fill(identities[0].name);
    dummyAuth(identities[0].dummyAuthIndex)(page);
    await dialog.getByRole("button", { name: "Upgrade identity" }).click();
    await managePage.assertVisible();
  });
});
