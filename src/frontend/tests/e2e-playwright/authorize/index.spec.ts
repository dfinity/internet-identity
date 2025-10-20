import { expect, test } from "@playwright/test";
import {
  authorize,
  authorizeWithUrl,
  clearStorage,
  createIdentity,
  dummyAuth,
  TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  II_URL,
  cancelDummyAuth,
} from "../utils";

const DEFAULT_USER_NAME = "John Doe";

test("Authorize by registering a new passkey", async ({ page }) => {
  const auth = dummyAuth();
  await authorize(page, async (authPage) => {
    await authPage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    await authPage.getByRole("button", { name: "Create new identity" }).click();
    await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
    auth(authPage);
    await authPage.getByRole("button", { name: "Create identity" }).click();
  });
});

test("Authorize by signing in with an existing passkey", async ({ page }) => {
  const auth = dummyAuth();
  await createIdentity(page, DEFAULT_USER_NAME, auth);
  await clearStorage(page);
  await authorize(page, async (authPage) => {
    await authPage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    auth(authPage);
    await authPage
      .getByRole("button", { name: "Use existing identity" })
      .click();
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
});

test("Authorize by signing in from another device", async ({
  browser,
  page,
}) => {
  // Create identity on other device
  const newContext = await browser.newContext();
  const otherDevicePage = await newContext.newPage();
  const authOtherDevice = dummyAuth();
  const expectedPrincipal = await createIdentity(
    otherDevicePage,
    DEFAULT_USER_NAME,
    authOtherDevice,
  );

  const principal = await authorize(page, async (authPage) => {
    // Switch to current device and start "Continue from another device" flow to get link
    await authPage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    cancelDummyAuth(authPage);
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
    authOtherDevice(otherDevicePage);
    await otherDevicePage
      .getByRole("button", { name: DEFAULT_USER_NAME })
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
    const authCurrentDevice = dummyAuth();
    authCurrentDevice(authPage);
    await authPage.getByRole("button", { name: "Create passkey" }).click();
    await authPage
      .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
      .waitFor({ state: "hidden" });

    // Switch to other device and verify we have two passkeys
    await otherDevicePage
      .getByRole("heading", { level: 1, name: "Continue on your new device" })
      .waitFor({ state: "hidden" });
    await expect(otherDevicePage.getByText("Chrome")).toHaveCount(2);

    // Switch to current device and verify we can authorize
    await authPage.getByRole("button", { name: "Primary account" }).click();
  });
  expect(principal).toBe(expectedPrincipal);
});

test("Authorize by signing up with OpenID", async ({ page }) => {
  const userId = crypto.randomUUID();

  // Set name claim
  await fetch(`http://localhost:11105/account/${userId}/claims`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ name: DEFAULT_USER_NAME }),
  });

  await authorize(page, async (authPage) => {
    // Pick OpenID to continue
    const pagePromise = authPage.context().waitForEvent("page");
    await authPage
      .getByRole("button", { name: "Continue with Test OpenID" })
      .click();

    // Authenticate and authorize with OpenID
    const openIdPage = await pagePromise;
    await expect(
      openIdPage.getByRole("heading", {
        name: "Sign-in",
      }),
    ).toBeVisible();
    await openIdPage.getByPlaceholder("Enter any login").fill(userId);
    await openIdPage
      .getByPlaceholder("and password")
      .fill("any-password-works");
    await openIdPage.getByRole("button", { name: "Sign-in" }).click();
    await expect(
      openIdPage.getByRole("heading", {
        name: "Authorize",
      }),
    ).toBeVisible();
    await openIdPage.getByRole("button", { name: "Continue" }).click();
    await openIdPage.waitForEvent("close", { timeout: 15_000 });
  });
});

test("Authorize with ICRC-29", async ({ page }) => {
  const auth = dummyAuth();
  await authorizeWithUrl(
    page,
    TEST_APP_URL,
    II_URL + "/authorize",
    async (authPage) => {
      await authPage
        .getByRole("button", { name: "Continue with passkey" })
        .click();
      await authPage
        .getByRole("button", { name: "Create new identity" })
        .click();
      await authPage.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
      auth(authPage);
      await authPage.getByRole("button", { name: "Create identity" }).click();
    },
    true,
  );
});

test("Authorize with ICRC-29 (directly through OpenID)", async ({ page }) => {
  const userId = crypto.randomUUID();

  // Set name claim
  await fetch(`http://localhost:11105/account/${userId}/claims`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ name: DEFAULT_USER_NAME }),
  });

  await authorizeWithUrl(
    page,
    TEST_APP_URL,
    II_URL + "/authorize?openid=test%20openid",
    async (openIdPage) => {
      await openIdPage.waitForURL("http://localhost:11105/interaction/*");
      await expect(
        openIdPage.getByRole("heading", {
          name: "Sign-in",
        }),
      ).toBeVisible();
      await openIdPage.getByPlaceholder("Enter any login").fill(userId);
      await openIdPage
        .getByPlaceholder("and password")
        .fill("any-password-works");
      await openIdPage.getByRole("button", { name: "Sign-in" }).click();
      await expect(
        openIdPage.getByRole("heading", {
          name: "Authorize",
        }),
      ).toBeVisible();
      await openIdPage.getByRole("button", { name: "Continue" }).click();
    },
    true,
  );
});

test("App logo appears when app is known", async ({ page }) => {
  const auth = dummyAuth();
  await authorizeWithUrl(page, TEST_APP_URL, II_URL, async (authPage) => {
    await expect(authPage.locator('img[alt*="logo"]')).toBeVisible();

    await authPage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    await authPage.getByRole("button", { name: "Create new identity" }).click();
    await authPage.getByLabel("Identity name").fill("John Doe");
    auth(authPage);
    await authPage.getByRole("button", { name: "Create identity" }).click();
  });
});

test("App logo doesn't appear when app is not known", async ({ page }) => {
  const auth = dummyAuth();
  await authorizeWithUrl(
    page,
    TEST_APP_CANONICAL_URL,
    II_URL,
    async (authPage) => {
      await expect(authPage.locator('[aria-hidden="true"] svg')).toBeVisible();
      await expect(authPage.locator('img[alt*="logo"]')).not.toBeVisible();

      await authPage
        .getByRole("button", { name: "Continue with passkey" })
        .click();
      await authPage
        .getByRole("button", { name: "Create new identity" })
        .click();
      await authPage.getByLabel("Identity name").fill("John Doe");
      auth(authPage);
      await authPage.getByRole("button", { name: "Create identity" }).click();
    },
  );
});
