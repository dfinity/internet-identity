import { runInBrowser } from "../../util";

import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { expect } from "vitest";
import { authorize, createPasskeyIdentity } from "./utils";

test("Create and authorize with additional account", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { credential, principal } = await createPasskeyIdentity({ browser });
    const { principal: otherPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("radio", { name: "Use another account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
        await page
          .getByRole("button", { name: "Create additional account" })
          .click();
        await page.getByLabel("Account name").input("Work account");
        await page.getByRole("button", { name: "Create account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).not.toBe(otherPrincipal);
  });
}, 300_000);

test("Create additional account but authorize with default account", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { credential, principal } = await createPasskeyIdentity({ browser });
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("radio", { name: "Use another account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
        await page
          .getByRole("button", { name: "Create additional account" })
          .click();
        await page.getByLabel("Account name").input("DeFi account");
        await page.getByRole("button", { name: "Create account" }).click();
        await page.getByRole("radio", { name: "Primary account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);

test("Create and authorize with additional account, then switch back to primary account", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { credential, principal } = await createPasskeyIdentity({ browser });
    const { principal: otherPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("radio", { name: "Use another account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
        await page
          .getByRole("button", { name: "Create additional account" })
          .click();
        await page.getByLabel("Account name").input("Private account");
        await page.getByRole("button", { name: "Create account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("radio", { name: "Use another account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
        await page.getByRole("radio", { name: "Primary account" }).click();
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).not.toBe(otherPrincipal);
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);
