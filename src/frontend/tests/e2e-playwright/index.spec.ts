import { expect } from "@playwright/test";
import {
  cancelDummyAuth,
  clearStorage,
  createIdentity,
  dummyAuth,
  II_URL,
} from "./utils";
import { test } from "./fixtures";
import { a } from "vitest/dist/chunks/suite.d.FvehnV49.js";

const DEFAULT_USER_NAME = "John Doe";
const SECONDARY_USER_NAME = "Jane Doe";

test.describe("First visit", () => {
  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    await page.getByRole("button", { name: "Create new identity" }).click();
    await page.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create identity" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("Sign in with an existing passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await clearStorage(page);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Sign in" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use existing identity" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("Sign in from another device", async ({ browser, page }) => {
    // Create identity on existing device
    const existingDevicePage = page; // Create alias variable for clarity
    const authExistingDevice = dummyAuth();
    await createIdentity(
      existingDevicePage,
      DEFAULT_USER_NAME,
      authExistingDevice,
    );

    // Switch to new device and start "Continue from another device" flow to get link
    const newContext = await browser.newContext();
    const newDevicePage = await newContext.newPage();
    await newDevicePage.goto(II_URL);
    await newDevicePage.getByRole("button", { name: "Sign in" }).click();
    await newDevicePage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    cancelDummyAuth(newDevicePage);
    await newDevicePage
      .getByRole("button", { name: "Use existing identity" })
      .click();
    await newDevicePage
      .getByRole("heading", {
        level: 1,
        name: "Can't find your identity?",
      })
      .waitFor();
    const linkToPair = `https://${await newDevicePage.getByLabel("Pairing link").innerText()}`;

    // Switch to existing device and authenticate after visiting link
    await existingDevicePage.goto(linkToPair);
    await existingDevicePage
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    authExistingDevice(existingDevicePage);
    await existingDevicePage
      .getByRole("button", { name: "Use existing identity" })
      .click();

    // Switch to new device and get confirmation code
    await newDevicePage.getByLabel("Confirmation Code").waitFor();
    await newDevicePage
      .getByRole("button", { name: "Generating code..." })
      .waitFor({ state: "hidden" });
    const confirmationCode = await newDevicePage
      .getByLabel("Confirmation Code")
      .innerText();
    const confirmationCodeArray = confirmationCode.split("");

    // Switch to existing device and enter confirmation code
    await existingDevicePage
      .getByRole("heading", { level: 1, name: "Authorize new device" })
      .waitFor();
    for (let i = 0; i < confirmationCodeArray.length; i++) {
      const code = confirmationCodeArray[i];
      await existingDevicePage.getByLabel(`Code input ${i}`).fill(code);
    }
    await existingDevicePage
      .getByRole("button", { name: "Confirm sign-in" })
      .click();
    await existingDevicePage
      .getByRole("heading", { level: 1, name: "Continue on your new device" })
      .waitFor();

    // Switch to new device and register new passkey
    await newDevicePage
      .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
      .waitFor();
    const authNewDevice = dummyAuth();
    authNewDevice(newDevicePage);
    await newDevicePage.getByRole("button", { name: "Create passkey" }).click();
    await newDevicePage
      .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
      .waitFor({ state: "hidden" });

    // Switch to existing device, navigate to access methods and verify we have two passkeys
    await existingDevicePage
      .getByRole("heading", { level: 1, name: "Continue on your new device" })
      .waitFor({ state: "hidden" });
    const existingMenuButton = existingDevicePage.getByRole("button", {
      name: "Open menu",
    });
    if (await existingMenuButton.isVisible()) {
      await existingMenuButton.click();
    }
    await existingDevicePage
      .getByRole("link", { name: "Access and recovery" })
      .click();
    await expect(existingDevicePage.getByText("Unknown")).toHaveCount(2);

    // Switch to new device and verify we are signed in
    await newDevicePage.waitForURL(II_URL + "/manage");
    await expect(
      newDevicePage.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test.describe("OpenID user with name claim", () => {
    // This is chosen on purpose to exhibit a JWT token that is encoded in base64url
    // but cannot be decoded as simply base64. Works as a regression test.
    const name = "įìęèéêêëėįì";

    test.use({
      openIdConfig: {
        createUsers: [
          {
            claims: { name },
          },
        ],
      },
    });

    test("Sign up with OpenID", async ({
      page,
      signInWithOpenId,
      openIdUsers,
    }) => {
      // Pick OpenID to continue
      await page.goto(II_URL);
      await page.getByRole("button", { name: "Sign in" }).click();
      const popupPromise = page.context().waitForEvent("page");
      await page
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();

      // Sign in on OpenID page
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      // Assert that dashboard is shown
      await page.waitForURL(II_URL + "/manage");
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${name}!`),
        }),
      ).toBeVisible();
    });
  });

  test.describe("OpenID user without name claim", () => {
    test.use({
      openIdConfig: {
        createUsers: [
          {
            claims: {},
          },
        ],
      },
    });

    test("Sign up with OpenID", async ({
      page,
      signInWithOpenId,
      openIdUsers,
    }) => {
      // Pick OpenID to continue
      await page.goto(II_URL);
      await page.getByRole("button", { name: "Sign in" }).click();
      const popupPromise = page.context().waitForEvent("page");
      await page
        .getByRole("button", { name: openIdUsers[0].issuer.name })
        .click();

      // Sign in on OpenID page
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      // Enter identity name
      const name = "John Doe";
      await page.getByLabel("Identity name").fill(name);
      await page.getByRole("button", { name: "Create identity" }).click();

      // Assert that dashboard is shown
      await page.waitForURL(II_URL + "/manage");
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${name}!`),
        }),
      ).toBeVisible();
    });
  });
});

test.describe("Last used identities listed", () => {
  test("Sign in with last used identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Switch identity" }).click();
    auth(page);
    await page.getByRole("button", { name: DEFAULT_USER_NAME }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("Sign in with another identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use existing identity" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
  });

  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with passkey" }).click();
    await page.getByRole("button", { name: "Create new identity" }).click();
    await page.getByLabel("Identity name").fill(SECONDARY_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create identity" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${SECONDARY_USER_NAME}!`),
      }),
    ).toBeVisible();
  });
});
