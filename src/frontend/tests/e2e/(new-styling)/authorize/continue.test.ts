import { runInBrowser, wipeStorage } from "../../util";

import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { authorize, createPasskeyIdentity } from "./utils";
import { expect } from "vitest";
import { II_URL } from "../../constants";

test("Authorize with last used account", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { principal, credential } = await createPasskeyIdentity({ browser });
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);

test("Authorize by switching to another identity", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { principal, credential } = await createPasskeyIdentity({
      browser,
      name: "Identity 1",
    });
    const { principal: otherPrincipal } = await createPasskeyIdentity({
      browser,
      name: "Identity 2",
    });
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("button", { name: "Switch identity" }).click();
        await page.getByRole("button", { name: "Identity 1" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(otherPrincipal).not.toBe(expectedPrincipal);
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);

// TODO: Mock discoverable passkeys so this test works as expected
test.skip("Authorize by signing in with a different passkey", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Create new passkey and then wipe the last used identities from storage
    const { credential, principal } = await createPasskeyIdentity({ browser });
    await browser.url(II_URL);
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
