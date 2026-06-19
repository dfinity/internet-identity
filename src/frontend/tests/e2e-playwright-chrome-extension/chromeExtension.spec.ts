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
    // /authorize renders the picker in mode="signin"; switch to sign-up.
    await authorizePage.page
      .getByRole("button", { name: "Create", exact: true })
      .click();
    await authorizePage.page
      .getByRole("button", { name: "Sign up with passkey" })
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
