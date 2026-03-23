import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  authorize,
  authorizeWithUrl,
  TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  II_URL,
  addVirtualAuthenticator,
} from "../../utils";
import { IdentityWizard } from "../../fixtures/identity";

test("Authorize with last used identity", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  // Sign in with an identity to have a last used identity
  const expectedPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber); // Identity isn't "last used" yet
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });

  // Authorize again and expect to be authorized with the same identity in continue flow
  const principal = await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(principal).toBe(expectedPrincipal);
});

test.describe("multiple identities", () => {
  test.use({
    identityConfig: {
      createIdentities: [{ name: "Test 1" }, { name: "Test 2" }],
    },
  });

  test("Authorize by switching to another identity", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    const expectedPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
    const otherPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[1].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
    // Verify that we can switch back to the first identity in the continue flow
    const principal = await authorize(page, async (authPage) => {
      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await authPage.getByRole("button", { name: "Switch identity" }).click();
      await authPage.getByRole("button", { name: identities[0].name }).click();
    });
    expect(principal).toBe(expectedPrincipal);
    expect(principal).not.toBe(otherPrincipal);
  });

  test("Authorize by signing in with a different passkey", async ({
    page,
    identities,
    addAuthenticatorForIdentity,
    signInWithIdentity,
  }) => {
    const expectedPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
    const otherPrincipal = await authorize(page, async (authPage) => {
      await signInWithIdentity(authPage, identities[1].identityNumber);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
    const principal = await authorize(page, async (authPage) => {
      await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
      await authPage.getByRole("button", { name: "Switch identity" }).click();
      await authPage
        .getByRole("button", { name: "Add another identity" })
        .click();
      await authPage
        .getByRole("button", { name: "Continue with passkey" })
        .click();
      await authPage
        .getByRole("button", { name: "Use existing identity" })
        .click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
    expect(principal).toBe(expectedPrincipal);
    expect(principal).not.toBe(otherPrincipal);
  });
});

test("Authorize by creating a new identity", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  const initialPrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  const newIdentityPrincipal = await authorize(page, async (authPage) => {
    await addVirtualAuthenticator(authPage);
    const wizard = new IdentityWizard(authPage);
    await wizard.signUp("Jane Doe");
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
  expect(newIdentityPrincipal).not.toBe(initialPrincipal);
});

test("App logo appears when app is known", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  await authorizeWithUrl(page, TEST_APP_URL, II_URL, async (authPage) => {
    await signInWithIdentity(authPage, identities[0].identityNumber);
    await expect(authPage.locator('img[alt*="logo"]')).toBeVisible();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
});

test("App logo doesn't appear when app is not known", async ({
  page,
  identities,
  signInWithIdentity,
}) => {
  await authorizeWithUrl(
    page,
    TEST_APP_CANONICAL_URL,
    II_URL,
    async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
      await expect(authPage.locator('[aria-hidden="true"] svg')).toBeVisible();
      await expect(authPage.locator('img[alt*="logo"]')).not.toBeVisible();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    },
  );
});
