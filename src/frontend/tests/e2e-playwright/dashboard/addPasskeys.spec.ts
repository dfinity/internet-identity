import { expect, test } from "@playwright/test";
import {
  addPasskeyCurrentDevice,
  clearStorage,
  createNewIdentityInII,
  dummyAuth,
  II_URL,
  renamePasskey,
  signOut,
} from "../utils";

const TEST_USER_NAME = "Test User";

test("User can log into the dashboard and add a new passkey from the same device and log in with it", async ({
  page,
  context,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  // Start the "add passkey" flow
  const auth2 = dummyAuth();
  await addPasskeyCurrentDevice(page, auth2);
  await expect(page.getByText("Chrome")).toHaveCount(2);
  await renamePasskey(page, "New Passkey");

  // Verify that the new passkey is not the current one
  await expect(
    page.getByText("Chrome").locator("..").getByLabel("Current Passkey"),
  ).toHaveCount(1);
  await expect(
    page.getByText("New Passkey").locator("..").getByLabel("Current Passkey"),
  ).toHaveCount(0);

  await signOut(page);

  // Clear storage and log in again with new passkey
  await clearStorage(page);
  const newPage = await context.newPage();
  await newPage.goto(II_URL);
  await newPage.getByRole("button", { name: "Continue with Passkey" }).click();
  auth2(newPage);
  await newPage
    .getByRole("button", { name: "Use an existing Passkey" })
    .click();

  await expect(
    newPage.getByText("Chrome").locator("..").getByLabel("Current Passkey"),
  ).toHaveCount(0);
  await expect(
    newPage
      .getByText("New Passkey")
      .locator("..")
      .getByLabel("Current Passkey"),
  ).toHaveCount(1);
  await newPage.close();
});

test("User can log in the dashboard and add a new passkey from another device", async ({
  page,
  browser,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use an existing Passkey" }).click();

  // Verify we're at the dashboard and have one passkey
  await page.waitForURL(II_URL + "/manage");
  await expect(page.getByText("Chrome")).toHaveCount(1);

  // Start the "add passkey" flow
  await page.getByRole("button", { name: "Add" }).click();
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  await page
    .getByRole("button", { name: "Continue on another device" })
    .click();

  const linkToPair = await page.getByLabel("Pairing link").innerText();

  const newContext = await browser.newContext();
  const linkPage = await newContext.newPage();
  await linkPage.goto(`https://${linkToPair}`);

  const authorizeNewDevicePromise = page
    .getByRole("heading", { level: 1, name: "Authorize new device" })
    .waitFor();

  await linkPage.getByLabel("Confirmation Code").waitFor();
  await linkPage
    .getByRole("button", { name: "Generating code..." })
    .waitFor({ state: "hidden" });
  await authorizeNewDevicePromise;

  const confirmationCode = await linkPage
    .getByLabel("Confirmation Code")
    .innerText();
  const confirmationCodeArray = confirmationCode.split("");

  for (let i = 0; i < confirmationCodeArray.length; i++) {
    const code = confirmationCodeArray[i];
    await page.getByLabel(`Code input ${i}`).fill(code);
  }

  await page.getByRole("button", { name: "Confirm sign-in" }).click();

  await page
    .getByRole("heading", { level: 1, name: "Continue on your new device" })
    .waitFor();

  await linkPage
    .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
    .waitFor();

  // Create and register new passkey
  const authLinkPage = dummyAuth();
  authLinkPage(linkPage);
  await linkPage.getByRole("button", { name: "Create passkey" }).click();

  await linkPage
    .getByRole("heading", { level: 1, name: "Confirm your sign-in" })
    .waitFor({ state: "hidden" });

  await page
    .getByRole("heading", { level: 1, name: "Continue on your new device" })
    .waitFor({ state: "hidden" });

  // Verify that we now have two passkeys
  await expect(page.getByText("Chrome")).toHaveCount(2);
});
