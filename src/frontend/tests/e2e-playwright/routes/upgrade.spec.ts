import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  fromBase64URL,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
  LEGACY_II_URL,
  removeVirtualAuthenticator,
  addCredentialToVirtualAuthenticator,
  CredentialIdentity,
  createActorForCredential,
} from "../utils";

const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

test("Can upgrade identity", async ({ page, managePage, identities }) => {
  // Navigate to legacy page that doesn't redirect
  await page.goto(LEGACY_II_URL + "/self-service");

  // Add virtual authenticator and create a passkey in page context
  const legacyAuthenticatorId = await addVirtualAuthenticator(page);
  await page.evaluate(
    async (rpId) => {
      await navigator.credentials.create({
        publicKey: {
          challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
          attestation: "direct",
          pubKeyCredParams: [{ type: "public-key", alg: -7 }], // ES256
          rp: {
            name: "Internet Identity Service",
            id: rpId,
          },
          user: {
            id: window.crypto.getRandomValues(new Uint8Array(16)),
            displayName: "Internet Identity User",
            name: "Internet Identity User",
          },
        },
      });
    },
    LEGACY_II_URL.replace(/^https?:\/\//, ""),
  );

  // Get the created credential from the virtual authenticator
  const [legacyCredential] = await getCredentialsFromVirtualAuthenticator(
    page,
    legacyAuthenticatorId,
  );
  await removeVirtualAuthenticator(page, legacyAuthenticatorId);

  // Use an actor to create a legacy passkey (not id.ai)
  // since this functionality is no longer available.
  const actor = await createActorForCredential(
    identities[0].host,
    identities[0].canisterId,
    identities[0].credentials[0],
  );

  // Add the legacy passkey to the identity
  const legacyIdentity =
    await CredentialIdentity.fromCredential(legacyCredential);
  await actor.authn_method_add(identities[0].identityNumber, {
    metadata: [
      ["alias", { String: LEGACY_PASSKEY_NAME }],
      ["origin", { String: LEGACY_II_URL }],
    ],
    authn_method: {
      WebAuthn: {
        pubkey: legacyIdentity.getPublicKey().toDer(),
        credential_id: fromBase64URL(legacyCredential.credentialId),
        aaguid: [],
      },
    },
    security_settings: {
      protection: { Unprotected: null },
      purpose: { Authentication: null },
    },
    last_authentication: [],
  });

  // Remove the non-legacy passkey so the identity is only accessible via the legacy passkey,
  // which simulates the state of an identity that hasn't been upgraded yet.
  const nonLegacyIdentity = await CredentialIdentity.fromCredential(
    identities[0].credentials[0],
  );
  await actor.authn_method_remove(
    identities[0].identityNumber,
    nonLegacyIdentity.getPublicKey().toDer(),
  );

  // Verify identity can be upgraded multiple times
  for (let attempt = 0; attempt < 3; attempt++) {
    // Navigate to the new II_URL to trigger the upgrade flow
    await page.goto(II_URL);
    const authenticatorId = await addVirtualAuthenticator(page);
    await addCredentialToVirtualAuthenticator(
      page,
      authenticatorId,
      legacyCredential,
    );

    // Open the sign-in dialog
    if (attempt > 0) {
      await page.getByRole("button", { name: "Switch identity" }).click();
      await page.getByRole("button", { name: "Add another identity" }).click();
    } else {
      await page.getByRole("button", { name: "Sign in" }).click();
    }

    // Select the passkey authentication method
    const dialog = page.getByRole("dialog");
    await dialog.getByRole("button", { name: "Continue with passkey" }).click();
    await dialog.getByRole("button", { name: "Upgrade" }).click();

    // Enter the identity number
    await dialog
      .getByPlaceholder("Internet Identity number")
      .fill(identities[0].identityNumber.toString());
    await dialog.getByRole("button", { name: "Continue" }).click();

    // On subsequent attempts, we expect a message that the identity is already upgraded
    if (attempt > 0) {
      await expect(
        dialog.getByRole("heading", { name: "Identity already upgraded" }),
      ).toBeVisible();
      await dialog.getByRole("button", { name: "Upgrade again" }).click();
    }

    // Complete the upgrade process
    await dialog.getByLabel("Identity name").fill(identities[0].name);
    await dialog.getByRole("button", { name: "Upgrade identity" }).click();
    await expect(dialog).toBeHidden();
    await managePage.assertVisible();

    // Cleanup by removing the newly added authenticator and signing out
    await managePage.signOut();
    await removeVirtualAuthenticator(page, authenticatorId);
  }
});
