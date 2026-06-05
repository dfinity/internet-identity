import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  authorizeWithUrl,
  fromBase64URL,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
  LEGACY_II_URL,
  TEST_APP_URL,
  removeVirtualAuthenticator,
  addCredentialToVirtualAuthenticator,
  CredentialIdentity,
  createActorForCredential,
} from "../utils";

const LEGACY_PASSKEY_ALIAS = "pre-upgrade-passkey";

/**
 * Creates a virtual authenticator scoped to LEGACY_II_URL, registers a
 * passkey credential against that RP ID, removes the authenticator, and
 * returns the raw CDP credential so callers can re-add it on demand.
 */
const createLegacyCredential = async (
  page: import("@playwright/test").Page,
) => {
  await page.goto(LEGACY_II_URL + "/self-service");
  const legacyAuthenticatorId = await addVirtualAuthenticator(page);
  await page.evaluate(
    async (rpId) => {
      await navigator.credentials.create({
        publicKey: {
          challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
          attestation: "direct",
          pubKeyCredParams: [{ type: "public-key", alg: -7 }],
          rp: { name: "Internet Identity Service", id: rpId },
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
  const [credential] = await getCredentialsFromVirtualAuthenticator(
    page,
    legacyAuthenticatorId,
  );
  await removeVirtualAuthenticator(page, legacyAuthenticatorId);
  return credential;
};

test.describe("/recovery — identity number entry point", () => {
  test("Identity number card is visible on the /recovery page", async ({
    page,
  }) => {
    await page.goto(II_URL + "/recovery");
    await expect(
      page.getByRole("button", { name: "Identity number", exact: true }),
    ).toBeVisible();
  });

  test("Clicking Identity number opens the MigrationWizard dialog", async ({
    page,
  }) => {
    await page.goto(II_URL + "/recovery");
    await page
      .getByRole("button", { name: "Identity number", exact: true })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByPlaceholder("Internet Identity number"),
    ).toBeVisible();
  });

  test("MigrationWizard dialog can be dismissed", async ({ page }) => {
    await page.goto(II_URL + "/recovery");
    await page
      .getByRole("button", { name: "Identity number", exact: true })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Close" }).click();
    await expect(dialog).toBeHidden();
    await expect(
      page.getByRole("button", { name: "Identity number", exact: true }),
    ).toBeVisible();
  });

  test("Completes the identity-number upgrade path and lands on /manage/access", async ({
    page,
    identities,
  }) => {
    // Prepare a legacy credential at LEGACY_II_URL's RP ID.
    const legacyCredential = await createLegacyCredential(page);

    // Use a test actor (authenticated as identities[0]) to register the
    // legacy passkey on the identity and remove the modern one, leaving only
    // the legacy passkey — the state of a not-yet-upgraded identity.
    const actor = await createActorForCredential(
      identities[0].host,
      identities[0].canisterId,
      identities[0].credentials[0],
    );

    const legacyIdentity =
      await CredentialIdentity.fromCredential(legacyCredential);
    await actor.authn_method_add(identities[0].identityNumber, {
      metadata: [
        ["alias", { String: LEGACY_PASSKEY_ALIAS }],
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

    const modernIdentity = await CredentialIdentity.fromCredential(
      identities[0].credentials[0],
    );
    await actor.authn_method_remove(
      identities[0].identityNumber,
      modernIdentity.getPublicKey().toDer(),
    );

    // Navigate to /recovery and open the identity-number wizard.
    await page.goto(II_URL + "/recovery");
    await page
      .getByRole("button", { name: "Identity number", exact: true })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();

    // Step 1 — enter identity number.
    await dialog
      .getByPlaceholder("Internet Identity number")
      .fill(identities[0].identityNumber.toString());

    // Add virtual authenticator with the legacy credential so WebAuthn
    // authentication against LEGACY_II_URL's RP ID succeeds.
    const authenticatorId = await addVirtualAuthenticator(page);
    await addCredentialToVirtualAuthenticator(
      page,
      authenticatorId,
      legacyCredential,
    );

    await dialog.getByRole("button", { name: "Continue" }).click();

    // Step 2 — name the new identity and create the upgrade passkey.
    await expect(
      dialog.getByRole("heading", { name: "Name your identity" }),
    ).toBeVisible();
    const upgradedName = identities[0].name + " (upgraded)";
    await dialog.getByLabel("Identity name").fill(upgradedName);
    await dialog.getByRole("button", { name: "Upgrade identity" }).click();

    // After success the dialog closes and we're redirected to /manage/access.
    await expect(dialog).toBeHidden();
    await page.waitForURL(II_URL + "/manage/access");
    await expect(
      page.getByRole("heading", { name: "Access methods" }),
    ).toBeVisible();

    await removeVirtualAuthenticator(page, authenticatorId);
  });
});

test.describe("AuthWizard passkey picker — no in-dialog upgrade link", () => {
  test("SetupOrUseExistingPasskey does not render an identity number upgrade link", async ({
    page,
  }) => {
    // Navigate to II landing page — first-time state shows the passkey picker inline.
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in with passkey" }).click();

    // The picker dialog / inline view must NOT contain the removed upgrade link.
    await expect(page.getByText("Still have an identity number?")).toBeHidden();
    await expect(page.getByRole("button", { name: "Upgrade" })).toBeHidden();
  });

  test("SetupOrUseExistingPasskey inside 'Add another identity' dialog has no upgrade link", async ({
    page,
    identities,
    signInWithIdentity,
  }) => {
    // Sign in to reach the welcome-back state where the picker is behind a dialog.
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);

    // Now on welcome-back state: open "Add identity" to reach picker.
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Add identity" }).click();
    await page.getByRole("button", { name: "Sign in with passkey" }).click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();

    await expect(
      dialog.getByText("Still have an identity number?"),
    ).toBeHidden();
    await expect(dialog.getByRole("button", { name: "Upgrade" })).toBeHidden();
  });
});

test.describe("/authorize — standalone upgrade panel sanity check", () => {
  test("Upgrade panel is present and opens the MigrationWizard when arriving from a legacy domain", async ({
    page,
  }) => {
    // Use the legacy II URL as the identity provider — visiting /authorize
    // from a non-primary origin triggers the GUIDED_UPGRADE feature flag,
    // causing the "Still using an identity number?" upgrade panel to render.
    await authorizeWithUrl(
      page,
      TEST_APP_URL,
      LEGACY_II_URL,
      async (authPage) => {
        await addVirtualAuthenticator(authPage);

        await expect(
          authPage.getByRole("paragraph").filter({
            hasText: "Still using an identity number?",
          }),
        ).toBeVisible({ timeout: 10000 });

        // Expand the panel if collapsed (MIN_GUIDED_UPGRADE flag may start it collapsed).
        const upgradeBtn = authPage.getByRole("button", {
          name: "Upgrade your identity",
        });
        const isExpanded = await upgradeBtn.isVisible();
        if (!isExpanded) {
          await authPage
            .getByRole("button", { name: "Upgrade" })
            .first()
            .click();
        }
        await upgradeBtn.click();

        // MigrationWizard should now be active — the identity number input is the gate.
        await expect(
          authPage.getByPlaceholder("Internet Identity number"),
        ).toBeVisible();

        // Cancel: close the wizard and complete the authorize flow by creating a new identity.
        await authPage.keyboard.press("Escape");
        await authPage
          .getByRole("button", { name: "Sign in with passkey" })
          .click();
        await authPage.getByLabel("Identity name").fill("Test");
        await authPage.getByRole("button", { name: "Create identity" }).click();
        await authPage
          .getByRole("button", { name: "Continue", exact: true })
          .click();
      },
    );
  });
});
