import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { authorizeWithUrl, TEST_APP_URL, II_URL } from "../../utils";

test("Re-authorizes with ?prompt=none without any interaction", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  // A normal sign-in first, which is what leaves Internet Identity holding a
  // delegation for this app.
  const expectedPrincipal = await authorizeWithUrl(
    page,
    TEST_APP_URL,
    `${II_URL}/authorize`,
    async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    },
    true,
  );

  // Nothing to do in the popup this time: no passkey, no virtual authenticator,
  // no Continue button. Internet Identity extends the delegation it kept and
  // closes without rendering anything, and the app ends up with the same
  // principal it had before.
  const principal = await authorizeWithUrl(
    page,
    TEST_APP_URL,
    `${II_URL}/authorize?prompt=none`,
    async () => {},
    true,
  );

  expect(principal).toBe(expectedPrincipal);
});
