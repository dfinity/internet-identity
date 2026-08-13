import { test, expect } from "@playwright/test";
import { Principal } from "@icp-sdk/core/principal";
import {
  II_URL,
  LEGACY_II_URL,
  ALT_LEGACY_II_URL,
  TEST_APP_URL,
  authorizeWithUrl,
  addVirtualAuthenticator,
  createActorForCredential,
  fromBase64,
  getCredentialsFromVirtualAuthenticator,
  type WebAuthnCredential,
} from "../../utils";
import { DEFAULT_HOST } from "../../fixtures/identity";
[LEGACY_II_URL, ALT_LEGACY_II_URL].forEach((legacyURL) => {
  test.describe(`Legacy domain ${legacyURL}`, () => {
    test(`sees upgrade banner during authentication`, async ({ page }) => {
      await authorizeWithUrl(
        page,
        TEST_APP_URL,
        legacyURL,
        async (authPage) => {
          // Assert that we've been redirected to non-legacy domain
          await expect(authPage).toHaveURL((url) => url.origin === II_URL);
          // Add virtual authenticator
          await addVirtualAuthenticator(authPage);
          // Assert that the user is informed about the upgrade
          await expect(
            authPage.getByRole("heading", {
              name: "has moved to the new Internet Identity",
            }),
          ).toBeVisible();
          // Create new identity and continue to app
          await authPage
            .getByRole("button", { name: "Create", exact: true })
            .click();
          await authPage
            .getByRole("button", { name: "Create with passkey" })
            .click();
          await authPage.getByLabel("Identity name").fill("Test");
          await authPage
            .getByRole("button", { name: "Create identity" })
            .click();
          await authPage
            .getByRole("button", { name: "Continue", exact: true })
            .click();
        },
      );
    });

    test(`creates the passkey for the primary origin when a dapp points the ICRC-25 transport at this domain`, async ({
      page,
    }) => {
      let canisterId: Principal | undefined;
      let credentials: WebAuthnCredential[] = [];

      await authorizeWithUrl(
        page,
        TEST_APP_URL,
        `${legacyURL}/authorize`,
        async (authPage) => {
          // The channel has to be established on the primary origin: the
          // ICRC-29 client pins the signer origin once the handshake
          // completes, so redirecting after that would strand the dapp.
          await expect(authPage).toHaveURL((url) => url.origin === II_URL);
          const authenticatorId = await addVirtualAuthenticator(authPage);
          const canisterIdAttribute = await authPage
            .locator("[data-canister-id]")
            .getAttribute("data-canister-id");
          if (canisterIdAttribute === null) {
            throw new Error("Canister id is missing from the II page");
          }
          canisterId = Principal.fromText(canisterIdAttribute);
          // Create new identity and continue to app
          await authPage
            .getByRole("button", { name: "Create", exact: true })
            .click();
          await authPage
            .getByRole("button", { name: "Create with passkey" })
            .click();
          await authPage.getByLabel("Identity name").fill("Test");
          await authPage
            .getByRole("button", { name: "Create identity" })
            .click();
          // Read the credential before the window closes, the virtual
          // authenticator is scoped to this page.
          credentials = await getCredentialsFromVirtualAuthenticator(
            authPage,
            authenticatorId,
          );
          await authPage
            .getByRole("button", { name: "Continue", exact: true })
            .click();
        },
        true,
      );

      if (canisterId === undefined || credentials.length === 0) {
        throw new Error("Identity was not created");
      }
      const actor = await createActorForCredential(
        DEFAULT_HOST,
        canisterId,
        credentials[0],
      );
      const [deviceKeyWithAnchor] = await actor.lookup_device_key(
        fromBase64(credentials[0].credentialId),
      );
      if (deviceKeyWithAnchor === undefined) {
        throw new Error("Created passkey is not registered with an identity");
      }
      const devices = await actor.lookup(deviceKeyWithAnchor.anchor_number);
      const passkeys = devices.filter(
        (device) => device.credential_id.length > 0,
      );
      expect(passkeys.length).toBeGreaterThan(0);
      // Assert all passkeys are created for the primary origin
      for (const passkey of passkeys) {
        expect(passkey.origin).toEqual([II_URL]);
      }
    });
  });
});
