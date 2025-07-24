import { expect, test } from "@playwright/test";
import { clearStorage, createIdentity, dummyAuth, II_URL } from "./utils";

const DEFAULT_USER_NAME = "John Doe";
const SECONDARY_USER_NAME = "Jane Doe";
const FEATURE_FLAG = "?feature_flag_continue_from_another_device=true";

test.describe("First visit", () => {
  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill(DEFAULT_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign in with an existing passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await clearStorage(page);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign in from another device", async ({ browser, page }) => {
    // Create identity on existing device
    const existingDevicePage = page;
    const authExistingDevice = dummyAuth();
    await createIdentity(
      existingDevicePage,
      DEFAULT_USER_NAME,
      authExistingDevice,
    );

    // Switch to new device and start "Continue from another device" flow to get link
    const newContext = await browser.newContext();
    const newDevicePage = await newContext.newPage();
    await newDevicePage.goto(II_URL + FEATURE_FLAG);
    await newDevicePage
      .getByRole("button", { name: "Continue from another device" })
      .click();
    const linkToPair = new URL(
      `https://${await newDevicePage.getByLabel("Pairing link").innerText()}`,
    );
    linkToPair.search = FEATURE_FLAG;
    const authorizeNewDevicePromise = existingDevicePage
      .getByRole("heading", { level: 1, name: "Authorize new device" })
      .waitFor();

    // Switch to existing device and authenticate after visiting link
    await existingDevicePage.goto(linkToPair.href);
    authExistingDevice(existingDevicePage);
    await existingDevicePage
      .getByRole("button", { name: DEFAULT_USER_NAME })
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
    await authorizeNewDevicePromise;
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

    // Switch to existing device and verify we have two passkeys
    await existingDevicePage
      .getByRole("heading", { level: 1, name: "Continue on your new device" })
      .waitFor({ state: "hidden" });
    await expect(existingDevicePage.getByText("Chrome")).toHaveCount(2);

    // Switch to new device and verify we can sign in
    authNewDevice(newDevicePage);
    await newDevicePage
      .getByRole("button", { name: DEFAULT_USER_NAME })
      .click();
    await newDevicePage.waitForURL(II_URL + "/manage");
    await expect(
      newDevicePage.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      newDevicePage.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });
});

test.describe("Last used identities listed", () => {
  test("Sign in with last used identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    auth(page);
    await page.getByRole("button", { name: DEFAULT_USER_NAME }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign in with another identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${DEFAULT_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: DEFAULT_USER_NAME }),
    ).toBeVisible();
  });

  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, DEFAULT_USER_NAME, auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill(SECONDARY_USER_NAME);
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
    await expect(
      page.getByRole("heading", {
        name: new RegExp(`Welcome, ${SECONDARY_USER_NAME}!`),
      }),
    ).toBeVisible();
    await expect(
      page.getByRole("heading", { level: 5, name: SECONDARY_USER_NAME }),
    ).toBeVisible();
  });
});
