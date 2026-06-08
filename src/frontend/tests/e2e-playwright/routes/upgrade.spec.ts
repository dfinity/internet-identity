import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  authorize,
  fromBase64URL,
  getCredentialsFromVirtualAuthenticator,
  LEGACY_II_URL,
  removeVirtualAuthenticator,
  addCredentialToVirtualAuthenticator,
  CredentialIdentity,
  createActorForCredential,
} from "../utils";

const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

test("Can upgrade identity", async ({ page, identities }) => {
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

  // The dedicated upgrade panel on /authorize is gated by the
  // GUIDED_UPGRADE feature flag, which only auto-enables when the page
  // loads on a non-primary origin (legacy domain). Persisting the
  // override in localStorage beats the domain-based default and gives
  // every popup opened in this context a visible "Upgrade" entry.
  await page.context().addInitScript(() => {
    try {
      window.localStorage.setItem(
        "ii-localstorage-feature-flags__GUIDED_UPGRADE",
        JSON.stringify(true),
      );
    } catch {
      // localStorage may be locked in some test contexts.
    }
  });

  // Verify identity can be upgraded multiple times via the dapp-driven
  // /authorize flow — the new canonical entry point for legacy users.
  for (let attempt = 0; attempt < 3; attempt++) {
    await authorize(page, async (authPage) => {
      const authenticatorId = await addVirtualAuthenticator(authPage);
      await addCredentialToVirtualAuthenticator(
        authPage,
        authenticatorId,
        legacyCredential,
      );

      // First attempt: the panel renders expanded with a primary
      // "Upgrade your identity" button. After the first success the
      // panel collapses (state persisted to `ii-guided-upgrade-collapsed`
      // in localStorage) and the entry becomes the secondary "Upgrade"
      // link in the collapsed header.
      const upgradeButton =
        attempt === 0
          ? authPage.getByRole("button", { name: "Upgrade your identity" })
          : authPage.getByRole("button", { name: "Upgrade", exact: true });
      await upgradeButton.click();

      const dialog = authPage.getByRole("dialog");
      await dialog
        .getByPlaceholder("Internet Identity number")
        .fill(identities[0].identityNumber.toString());
      await dialog.getByRole("button", { name: "Continue" }).click();

      // On subsequent attempts the identity is already migrated; the
      // wizard surfaces the "already upgraded" view with an explicit
      // "Upgrade again" CTA.
      if (attempt > 0) {
        await expect(
          dialog.getByRole("heading", { name: "Identity already upgraded" }),
        ).toBeVisible();
        await dialog.getByRole("button", { name: "Upgrade again" }).click();
      }

      // Complete the upgrade. The wizard creates a fresh passkey in the
      // authenticator and routes to the success view.
      await dialog.getByLabel("Identity name").fill(identities[0].name);
      await dialog.getByRole("button", { name: "Upgrade identity" }).click();

      // Cleanup the popup's authenticator before driving the success
      // view forward — the next iteration spins up a fresh authenticator
      // and re-attaches the legacy credential alone.
      await removeVirtualAuthenticator(authPage, authenticatorId);

      // The success view auto-redirects to the dapp after a 5s
      // countdown; clicking is faster and matches a real user.
      await authPage.getByRole("button", { name: /Go to the app/ }).click();
    });
  }
});
