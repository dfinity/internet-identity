import { expect } from "@playwright/test";
import { chromeExtensionTest } from "../e2e-playwright/fixtures/chrome-extension";
import { addVirtualAuthenticator } from "../e2e-playwright/utils";

const authorizeTest = chromeExtensionTest.extend({
  authorizeConfig: ({ extensionUrl }, use) =>
    use({
      protocol: "legacy",
      testAppURL: extensionUrl,
    }),
});

authorizeTest.describe("Authorize from chrome extension", () => {
  authorizeTest.afterEach(({ authorizedPrincipal }) => {
    expect(authorizedPrincipal?.isAnonymous()).toBe(false);
  });

  authorizeTest("should authenticate", async ({ authorizePage }) => {
    await addVirtualAuthenticator(authorizePage.page);
    await authorizePage.page
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authorizePage.page
      .getByRole("button", { name: "Create new identity" })
      .click();
    await authorizePage.page.getByLabel("Identity name").fill("Extension User");
    await authorizePage.page
      .getByRole("button", { name: "Create identity" })
      .click();
    await authorizePage.page
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
});
