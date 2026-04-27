import { expect } from "@playwright/test";
import {
  authorize,
  authorizeWithUrl,
  TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  II_URL,
  addVirtualAuthenticator,
} from "../../utils";
import { test } from "../../fixtures";
import { SSO_OPENID_PORT } from "../../fixtures/sso";

const DEFAULT_USER_NAME = "John Doe";

test("Authorize by registering a new passkey", async ({ page }) => {
  await authorize(page, async (authPage) => {
    await addVirtualAuthenticator(authPage);
    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authPage.getByRole("button", { name: "Create new identity" }).click();
    await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
    await authPage.getByRole("button", { name: "Create identity" }).click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
});

test("Authorize by signing in with an existing passkey", async ({
  page,
  identities,
  addAuthenticatorForIdentity,
}) => {
  await authorize(page, async (authPage) => {
    await addAuthenticatorForIdentity(authPage, identities[0].identityNumber);
    await authPage
      .getByRole("button", { name: "Continue with Passkey" })
      .click();
    await authPage
      .getByRole("button", { name: "Use existing identity" })
      .click();
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });
});

test("Authorize by signing in from another device", async ({
  browser,
  page,
  identities,
  addAuthenticatorForIdentity,
  signInWithIdentity,
}) => {
  // Create a separate context to simulate another device
  const newContext = await browser.newContext();

  try {
    // Get principal from authorizing on other device
    const otherDevicePage = await newContext.newPage();
    await addAuthenticatorForIdentity(
      otherDevicePage,
      identities[0].identityNumber,
    );
    const expectedPrincipal = await authorize(
      otherDevicePage,
      async (authPage) => {
        await signInWithIdentity(authPage, identities[0].identityNumber);
        await authPage
          .getByRole("button", { name: "Continue", exact: true })
          .click();
      },
    );

    const principal = await authorize(page, async (authPage) => {
      // Switch to current device and start "Continue from another device" flow to get link
      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Continue with Passkey" })
        .click();
      await authPage
        .getByRole("button", {
          name: "Use existing identity",
        })
        .click();
      await authPage
        .getByRole("heading", {
          level: 1,
          name: "Can't find your identity?",
        })
        .waitFor();
      const linkToPair = `https://${await authPage.getByLabel("Pairing link").innerText()}`;

      // Switch to other device and authenticate after visiting link
      await otherDevicePage.goto(linkToPair);
      await otherDevicePage
        .getByRole("button", { name: "Continue with Passkey" })
        .click();
      await otherDevicePage
        .getByRole("button", { name: "Use existing identity" })
        .click();

      // Switch to current device and get confirmation code
      await authPage.getByLabel("Confirmation Code").waitFor();
      await authPage
        .getByRole("button", { name: "Generating code..." })
        .waitFor({ state: "hidden" });
      const confirmationCode = await authPage
        .getByLabel("Confirmation Code")
        .innerText();
      const confirmationCodeArray = confirmationCode.split("");

      // Switch to other device and enter confirmation code
      await otherDevicePage
        .getByRole("heading", { level: 1, name: "Authorize new device" })
        .waitFor();
      for (let i = 0; i < confirmationCodeArray.length; i++) {
        const code = confirmationCodeArray[i];
        await otherDevicePage.getByLabel(`Code input ${i}`).fill(code);
      }
      await otherDevicePage
        .getByRole("button", { name: "Confirm sign-in" })
        .click();
      await otherDevicePage
        .getByRole("heading", { level: 1, name: "Continue on your new device" })
        .waitFor();

      // Switch to current device and register new passkey
      await authPage
        .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
        .waitFor();
      await authPage.getByRole("button", { name: "Create passkey" }).click();
      await authPage
        .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
        .waitFor({ state: "hidden" });

      // Switch to other device
      await otherDevicePage
        .getByRole("heading", { level: 1, name: "Continue on your new device" })
        .waitFor({ state: "hidden" });

      // Navigate to access methods
      const menuButton = otherDevicePage.getByRole("button", {
        name: "Open menu",
      });
      if (await menuButton.isVisible()) {
        await menuButton.click();
      }
      await otherDevicePage
        .getByRole("link", { name: "Access and recovery" })
        .click();

      // Verify we have two passkeys
      await expect(otherDevicePage.getByText("Unknown")).toHaveCount(2);

      // Switch to current device and verify we can authorize
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    // Assert that the principals from both devices match
    expect(principal).toBe(expectedPrincipal);
  } finally {
    await newContext.close();
  }
});

test.describe("Sign up with OpenID", () => {
  test.use({
    openIdConfig: {
      createUsers: [
        {
          claims: { name: DEFAULT_USER_NAME },
        },
      ],
    },
  });

  test("Authorize by signing up with OpenID", async ({
    page,
    openIdUsers,
    signInWithOpenId,
  }) => {
    await authorize(page, async (authPage) => {
      // Pick OpenID to continue
      const pagePromise = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();

      // Authenticate and authorize with OpenID
      const openIdPage = await pagePromise;
      const closePromise = openIdPage.waitForEvent("close", {
        timeout: 15_000,
      });
      await signInWithOpenId(openIdPage, openIdUsers[0].id);
      await closePromise;

      // Continue to dapp
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

test.describe("Sign up with SSO", () => {
  test.use({
    openIdConfig: {
      defaultPort: SSO_OPENID_PORT,
      createUsers: [
        {
          claims: { name: DEFAULT_USER_NAME },
        },
      ],
    },
  });

  test("Authorize by signing up with SSO", async ({
    page,
    openIdUsers,
    openSsoPopup,
    signInWithOpenId,
  }) => {
    await authorize(page, async (authPage) => {
      // Pick SSO entry, type the discovery domain, wait for two-hop
      // discovery, then drive the IdP popup the same way as direct OpenID.
      const ssoPage = await openSsoPopup(authPage);
      const closePromise = ssoPage.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(ssoPage, openIdUsers[0].id);
      await closePromise;

      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });
});

test("Authorize with ICRC-29", async ({ page }) => {
  await authorizeWithUrl(
    page,
    TEST_APP_URL,
    II_URL + "/authorize",
    async (authPage) => {
      await addVirtualAuthenticator(authPage);
      await authPage
        .getByRole("button", { name: "Continue with Passkey" })
        .click();
      await authPage
        .getByRole("button", { name: "Create new identity" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    },
    true,
  );
});

test("App logo doesn't appear when app is not known", async ({ page }) => {
  await authorizeWithUrl(
    page,
    TEST_APP_CANONICAL_URL,
    II_URL,
    async (authPage) => {
      await addVirtualAuthenticator(authPage);
      await expect(authPage.locator('img[alt*="logo"]')).not.toBeVisible();

      await authPage
        .getByRole("button", { name: "Continue with Passkey" })
        .click();
      await authPage
        .getByRole("button", { name: "Create new identity" })
        .click();
      await authPage.getByLabel("Identity name").fill("John Doe");
      await authPage.getByRole("button", { name: "Create identity" }).click();
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    },
  );
});
