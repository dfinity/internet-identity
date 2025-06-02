import { runInBrowser, wipeStorage } from "../../util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { II_URL } from "../../constants";
import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { expect } from "vitest";
import { authorize, createPasskeyIdentity } from "./utils";

test("Authorize with a new passkey", async () => {
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

test("Authorize with an existing passkey", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Create new passkey and then wipe the last used identities from storage
    const { credential, principal } = await createPasskeyIdentity({ browser });
    await browser.url(II_URL + "#do-not-redirect");
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

test("Authorize with a different passkey", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Create new passkey and then wipe the last used identities from storage
    const { credential, principal } = await createPasskeyIdentity({ browser });
    await browser.url(II_URL + "#do-not-redirect");
    await wipeStorage(browser);
    // Create second passkey identity
    const { principal: otherPrincipal } = await createPasskeyIdentity({
      browser,
    });
    // Switch to first passkey identity from second passkey identity
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("button", { name: "Switch identity" }).click();
        await page
          .getByRole("link", { name: "Use another Internet Identity" })
          .click();
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
    expect(principal).not.toBe(otherPrincipal);
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);
