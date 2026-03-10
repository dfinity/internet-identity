import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  cdpPrivateKeyToPublicKeyDer,
  decodeCdpCredentialId,
  dummyAuth,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
  LEGACY_II_URL,
} from "../utils";
import { toSeed } from "../fixtures/identity";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";

const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

test("Can upgrade identity", async ({
  page,
  managePage,
  identities,
  actorForIdentity,
}) => {
  // Navigate to legacy page that doesn't redirect
  await page.goto(LEGACY_II_URL + "/self-service");

  // Add virtual authenticator and create a passkey in page context
  const authenticatorId = await addVirtualAuthenticator(page);
  await page.evaluate(
    async (rpId) => {
      await navigator.credentials.create({
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
  const [credential] = await getCredentialsFromVirtualAuthenticator(
    page,
    authenticatorId,
  );
  const publicKey = cdpPrivateKeyToPublicKeyDer(credential!.privateKey);
  const credentialId = decodeCdpCredentialId(credential!.credentialId);

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
        pubkey: publicKey,
        credential_id: credentialId,
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

  // Verify identity can be upgraded multiple times
  for (let attempt = 0; attempt < 3; attempt++) {
    // Navigate to the new II_URL to trigger the upgrade flow
    await page.goto(II_URL);

    // Open the sign-in dialog
    const dialog = page.getByRole("dialog");
    await page.getByRole("button", { name: "Sign in" }).click();
    await expect(dialog).toBeVisible();

    // Select the passkey authentication method
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
        page.getByRole("heading", { name: "Identity already upgraded" }),
      ).toBeVisible();
      await dialog.getByRole("button", { name: "Upgrade again" }).click();
    }

    // Set the identity name and authenticate with dummy auth
    const newAuth = dummyAuth();
    await dialog.getByLabel("Identity name").fill(identities[0].name);
    newAuth(page);

    // Complete the upgrade process
    await dialog.getByRole("button", { name: "Upgrade identity" }).click();
    await managePage.assertVisible();
  }
});
