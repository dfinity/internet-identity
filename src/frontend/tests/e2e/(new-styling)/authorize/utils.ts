import {
  addWebAuthnCredential,
  mockDiscoverablePasskeys,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  switchToPopup,
  waitToClose,
} from "../../util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { II_URL, TEST_APP_NICE_URL } from "../../constants";
import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { DemoAppView } from "../../views";
import { WebAuthnCredential } from "../../../e2e-setup";
import { nonNullish } from "@dfinity/utils";
import { expect } from "vitest";

/**
 * Authorize with test app
 * @returns passkey credential
 */
export const authorize = async (
  authenticate: (authenticator: string) => Promise<void>,
  {
    browser,
    credential,
  }: { browser: WebdriverIO.Browser; credential?: WebAuthnCredential },
): Promise<{ credential: WebAuthnCredential; principal: string }> => {
  // Open demo app and assert that user isn't authenticated yet
  const demoAppView = new DemoAppView(browser);
  await demoAppView.open(
    TEST_APP_NICE_URL,
    II_URL + "?feature_flag_discoverable_passkey_flow=true",
  );
  await demoAppView.waitForDisplay();
  expect(await demoAppView.getPrincipal()).toBe("");

  // Open II window
  await demoAppView.signin();
  const authenticatorId = await switchToPopup(browser);

  // Register existing passkey credential (if argument is passed),
  // additionally apply mock as workaround for discoverable passkeys.
  if (nonNullish(credential)) {
    await addWebAuthnCredential(
      browser,
      authenticatorId,
      credential,
      originToRelyingPartyId(II_URL),
    );
  }
  await mockDiscoverablePasskeys(browser);

  // Authenticate (with supplied argument fn)
  await authenticate(authenticatorId);
  const credentials = await getWebAuthnCredentials(browser, authenticatorId);
  await waitToClose(browser);

  // Assert that the user is authenticated
  const principal = await demoAppView.getPrincipal();
  expect(principal).not.toBe("");

  return { principal, credential: credentials[0] };
};

/**
 * Authorize with test app to create a passkey
 * @returns passkey credential
 */
export const createPasskeyIdentity = ({
  browser,
  name = "Jane Doe",
}: {
  browser: WebdriverIO.Browser;
  name?: string;
}): Promise<{ credential: WebAuthnCredential; principal: string }> =>
  authorize(
    async () => {
      const page = WebDriverPageObjectElement.create(browser);
      await page.getByRole("heading", { name: "Sign in" }).waitFor();
      const switchIdentity = page.getByRole("button", {
        name: "Switch identity",
      });
      if (await switchIdentity.isPresent()) {
        await switchIdentity.click();
        await page
          .getByRole("link", { name: "Use another Internet Identity" })
          .click();
      }
      await page.getByRole("button", { name: "Continue with Passkey" }).click();
      await page.getByRole("button", { name: "Set up a new Passkey" }).click();
      await page.getByLabel("Identity name").input(name);
      await page.getByRole("button", { name: "Create Passkey" }).click();
    },
    { browser },
  );
