import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  type AuthorizeConfig,
  performAuthorize,
} from "../../fixtures/authorize";
import { II_URL, TEST_APP_URL } from "../../utils";

const config: Partial<AuthorizeConfig> = {
  protocol: "icrc25",
  testAppURL: TEST_APP_URL,
  internetIdentityURL: II_URL,
};

test("Re-issues a delegation with ?prompt=none without any interaction", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  // A normal sign-in first: that is what leaves Internet Identity holding a
  // delegation for this app.
  await performAuthorize(page, config, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const expectedPrincipal = await page.locator("#principal").textContent();
  expect(expectedPrincipal).not.toBe("");

  // Nothing to do in the popup this time: no passkey, no virtual authenticator,
  // no Continue button. Internet Identity extends the delegation it kept and
  // closes without rendering anything, and the app ends up with the principal it
  // had before.
  await performAuthorize(page, { ...config, prompt: "none" }, async () => {});

  await expect(page.locator("#principal")).toHaveText(expectedPrincipal ?? "");
});
