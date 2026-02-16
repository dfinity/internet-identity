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

test("User can log into the dashboard and add a new passkey from the same device and log in with it after clearing storage", async ({
  page,
  context,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey and rename it
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);
  await renamePasskey(page, "Unknown", "Old passkey");

  // Start the "add passkey" flow
  const auth2 = dummyAuth();
  await addPasskeyCurrentDevice(page, auth2);
  await renamePasskey(page, "Unknown", "New passkey");

  // Verify we have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Verify that the new passkey is not the one currently in use
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Old passkey" })
      .getByText("Right now"),
  ).toBeVisible();
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "New passkey" })
      .getByText("Right now"),
  ).toBeHidden();

  await signOut(page);

  // Clear storage and log in again with new passkey
  await clearStorage(page);
  const newPage = await context.newPage();
  await newPage.goto(II_URL);
  await newPage.getByRole("button", { name: "Sign in" }).click();
  await newPage.getByRole("button", { name: "Continue with passkey" }).click();
  auth2(newPage);
  await newPage.getByRole("button", { name: "Use existing identity" }).click();
  await newPage.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const newMenuButton = newPage.getByRole("button", { name: "Open menu" });
  if (await newMenuButton.isVisible()) {
    await newMenuButton.click();
  }
  await newPage.getByRole("link", { name: "Access and recovery" }).click();

  // Verify that new passkey is the one currently in use
  await expect(
    newPage
      .getByRole("listitem")
      .filter({ hasText: "Old passkey" })
      .getByText("Right now"),
  ).toBeHidden();
  await expect(
    newPage
      .getByRole("listitem")
      .filter({ hasText: "New passkey" })
      .getByText("Right now"),
  ).toBeVisible();
  await newPage.close();
});

test("User can log in the dashboard and add a new passkey from another device", async ({
  page,
  browser,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey
  await expect(page.getByText("Unknown")).toHaveCount(1);

  // Start the "add passkey" flow
  await page.getByRole("button", { name: "Add new" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
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
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);
});

test("User can add a new passkey and use it with cached identity without clearing storage", async ({
  page,
  context,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey and rename it
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);
  await renamePasskey(page, "Unknown", "Old passkey");

  // Start the "add passkey" flow
  const auth2 = dummyAuth();
  await addPasskeyCurrentDevice(page, auth2);
  await renamePasskey(page, "Unknown", "New Passkey");

  // Verify we have two passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(2);

  // Verify that the new passkey is not the one currently in use
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "Old passkey" })
      .getByText("Right now"),
  ).toBeVisible();
  await expect(
    page
      .getByRole("listitem")
      .filter({ hasText: "New passkey" })
      .getByText("Right now"),
  ).toBeHidden();

  await signOut(page);

  // Log in again with new passkey WITHOUT clearing storage (key difference)
  // This should use the cached identity
  const newPage = await context.newPage();
  await newPage.goto(II_URL);
  await newPage.getByRole("button", { name: "Switch identity" }).click();

  // Click on the cached identity button directly
  // But use the new passkey to authenticate
  auth2(newPage);
  await newPage.getByRole("button", { name: "Manage identity" }).click();

  // Verify we're logged in with the new passkey
  await newPage.waitForURL(II_URL + "/manage");
  await expect(
    newPage.getByRole("heading", {
      name: new RegExp(`Welcome, ${TEST_USER_NAME}!`),
    }),
  ).toBeVisible();

  await newPage.close();
});

test("User can log into the dashboard and add up to 15 additional passkeys", async ({
  page,
}) => {
  const auth = dummyAuth();
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await createNewIdentityInII(page, TEST_USER_NAME, auth);
  await page.waitForURL(II_URL + "/manage");
  await clearStorage(page);
  await page.goto(II_URL);
  await page.getByRole("button", { name: "Sign in" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  auth(page);
  await page.getByRole("button", { name: "Use existing identity" }).click();

  // Verify we're at the dashboard
  await page.waitForURL(II_URL + "/manage");

  // Navigate to access methods
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Access and recovery" }).click();

  // Verify we have one passkey
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(1);

  // Add 15 more passkeys
  for (let i = 0; i < 15; i++) {
    await addPasskeyCurrentDevice(page, dummyAuth());
  }

  // Verify we have 16 passkeys
  await expect(
    page.getByRole("listitem").filter({ hasText: "Passkey" }),
  ).toHaveCount(16);

  // Verify we cannot add more passkeys
  await page.getByRole("button", { name: "Add new" }).click();
  await expect(
    page.getByRole("button", { name: "Continue with passkey" }),
  ).toBeDisabled();
});
