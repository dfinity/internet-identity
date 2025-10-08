import { expect, test } from "../fixtures";
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
  test.skip(
    ({ browserName }) => browserName === "webkit",
    "Migration test not supported on Safari because it uses virtual authenticators which are not supported.",
  );

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
        await authPage.getByRole("button", { name: "Upgrade" }).click();
        const authAuthenticatorId = await addVirtualAuthenticator(authPage);
        await addCredentialToVirtualAuthenticator(
          authPage,
          authAuthenticatorId,
          credential,
        );
        await authPage
          .getByPlaceholder("Internet Identity number")
          .fill(identityNumber);
        await authPage.getByRole("button", { name: "Continue" }).click();

        await authPage.getByLabel("Identity name").fill(TEST_USER_NAME);
        auth(authPage);
        await authPage.getByRole("button", { name: "Create Passkey" }).click();
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
        await authPage.getByRole("button", { name: "Primary account" }).click();
      },
    );
    expect(legacyPrincipal).toEqual(secondAuthPrincipal);
  });
});
