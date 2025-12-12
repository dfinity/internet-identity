import { expect, test } from "@playwright/test";
import type Protocol from "devtools-protocol";
import {
  addCredentialToVirtualAuthenticator,
  addVirtualAuthenticator,
  authorizeWithUrl,
  dummyAuth,
  getCredentialsFromVirtualAuthenticator,
  II_URL,
  LEGACY_II_URL,
  TEST_APP_URL,
} from "../utils";
import { isNullish } from "@dfinity/utils";

const TEST_USER_NAME = "Test User";

test.describe("Migration from an app", () => {
  test("User can migrate a legacy identity", async ({ page }) => {
    const auth = dummyAuth();
    let credential: Protocol.WebAuthn.Credential | undefined;
    let identityNumber: string | undefined = undefined;
    const legacyPrincipal = await authorizeWithUrl(
      page,
      TEST_APP_URL,
      LEGACY_II_URL,
      async (authPage) => {
        const authenticatorId = await addVirtualAuthenticator(authPage);

        await authPage
          .getByRole("button", { name: "Create Internet Identity" })
          .click();
        // Needs more time to load.
        await expect(authPage.locator("#userNumber")).toBeVisible({
          timeout: 15_000,
        });
        identityNumber = await authPage.locator("#userNumber").innerText();
        const credentials = await getCredentialsFromVirtualAuthenticator(
          authPage,
          authenticatorId,
        );
        credential = credentials[0];
        expect(credentials.length).toBeGreaterThan(0);
        await authPage
          .getByRole("button", { name: "I saved it, continue" })
          .click();
      },
    );
    const migratedPrincipal = await authorizeWithUrl(
      page,
      TEST_APP_URL,
      II_URL,
      async (authPage) => {
        if (isNullish(credential) || isNullish(identityNumber)) {
          throw new Error("Credential or identity number not found");
        }
        // Step 3: Perform the migration
        await authPage
          .getByRole("button", { name: "Continue with passkey" })
          .click();
        const dialog = authPage.getByRole("dialog");
        await expect(dialog).toBeVisible();
        await dialog.getByRole("button", { name: "Upgrade" }).click();
        const authAuthenticatorId = await addVirtualAuthenticator(authPage);
        await addCredentialToVirtualAuthenticator(
          authPage,
          authAuthenticatorId,
          credential,
        );
        await dialog
          .getByPlaceholder("Internet Identity number")
          .fill(identityNumber);
        await dialog.getByRole("button", { name: "Continue" }).click();

        await dialog.getByLabel("Identity name").fill(TEST_USER_NAME);
        auth(authPage);
        await dialog.getByRole("button", { name: "Upgrade identity" }).click();
      },
    );
    expect(legacyPrincipal).toEqual(migratedPrincipal);

    const secondAuthPrincipal = await authorizeWithUrl(
      page,
      TEST_APP_URL,
      II_URL,
      async (authPage) => {
        if (isNullish(credential) || isNullish(identityNumber)) {
          throw new Error("Credential or identity number not found");
        }
        auth(authPage);
        await authPage
          .getByRole("button", { name: "Continue", exact: true })
          .click();
      },
    );
    expect(legacyPrincipal).toEqual(secondAuthPrincipal);
  });
});
