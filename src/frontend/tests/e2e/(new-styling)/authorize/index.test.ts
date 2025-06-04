import { runInBrowser, wipeStorage } from "../../util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { II_URL } from "../../constants";
import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { expect } from "vitest";
import { authorize, createPasskeyIdentity } from "./utils";

test("Authorize by registering a new passkey", async () => {
  await runInBrowser(async (browser) => {
    await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page
          .getByRole("button", { name: "Continue with Passkey" })
          .click();
        await page
          .getByRole("button", { name: "Set up a new Passkey" })
          .click();
        await page.getByLabel("Identity name").input("John Doe");
        await page.getByRole("button", { name: "Create Passkey" }).click();
      },
      { browser },
    );
  });
}, 300_000);

// TODO: Mock discoverable passkeys so this test works as expected
test.skip("Authorize by signing in with an existing passkey", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Create new passkey and then wipe the last used identities from storage
    const { credential, principal } = await createPasskeyIdentity({ browser });
    await browser.url(II_URL);
    await wipeStorage(browser);
    // Authorize with the above passkey
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page
          .getByRole("button", { name: "Continue with Passkey" })
          .click();
        await page
          .getByRole("button", { name: "Use an existing Passkey" })
          .click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);
