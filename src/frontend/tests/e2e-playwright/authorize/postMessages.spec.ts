import { expect, test } from "@playwright/test";
import {
  createNewIdentityInII,
  dummyAuth,
  getMessageText,
  II_URL,
  openIiTab,
  openTestAppWithII,
  waitForNthMessage,
} from "../utils";

test("Authorize ready message should be sent immediately", async ({ page }) => {
  await openTestAppWithII(page);
  const iiPage = await openIiTab(page);
  await waitForNthMessage(page, 1);
  const messageText = await getMessageText(page, 1);
  expect(messageText).toContain("authorize-ready");
  await iiPage.close();
});

test("Should allow valid message", async ({ page }) => {
  const auth = dummyAuth();

  await openTestAppWithII(page);
  const iiPage = await openIiTab(page);

  // Wait for authorize-ready message, then send valid message
  await waitForNthMessage(page, 1); // message 1: authorize-ready
  await page.locator("#validMessageBtn").click(); // message 2: authorize-client

  // Complete authentication in II popup
  await createNewIdentityInII(iiPage, "Test User", auth);
  await iiPage.getByRole("button", { name: "Continue", exact: true }).click();

  // Wait for success notification in II
  await iiPage
    .getByRole("heading", { name: "Authentication successful" })
    .waitFor({ timeout: 15_000 });

  // Setup the listener because clicking closes the page immediately
  // and we might not add the listener on time.
  const closePromise = iiPage.waitForEvent("close");
  await iiPage.getByRole("button", { name: "Return to app" }).click();
  await closePromise;

  // Verify success message and expiration in test app
  await waitForNthMessage(page, 3); // message 3: authorize-success
  const successMessage = await getMessageText(page, 3);
  expect(successMessage).toContain("authorize-client-success");

  // Wait for principal to be populated (indicating successful authentication)
  await expect(page.locator("#principal")).not.toBeEmpty();
});

test("Should show error for invalid message", async ({ page }) => {
  await openTestAppWithII(page);
  const iiPage = await openIiTab(page);

  // Wait for authorize-ready message, then send invalid data
  await waitForNthMessage(page, 1); // message 1: authorize-ready
  await page.locator("#invalidDataBtn").click(); // Send invalid post message

  // Check for specific error message in II popup
  await expect(
    iiPage.getByRole("heading", { name: "Unable to connect" }),
  ).toBeVisible();

  // Setup the listener because clicking closes the page immediately
  // and we might not add the listener on time.
  const closePromise = iiPage.waitForEvent("close");
  await iiPage.getByRole("button", { name: "Return to app" }).click();
  await closePromise;
});

test("Should show error after not receiving message for 10 seconds", async ({
  page,
}) => {
  await openTestAppWithII(page);
  const iiPage = await openIiTab(page);

  await new Promise((resolve) => setTimeout(resolve, 10_000));

  // Check for specific error message in II popup
  await expect(
    iiPage.getByRole("heading", { name: "Unable to connect" }),
  ).toBeVisible();

  // Setup the listener because clicking closes the page immediately
  // and we might not add the listener on time.
  const closePromise = iiPage.waitForEvent("close");
  await iiPage.getByRole("button", { name: "Return to app" }).click();
  await closePromise;
});

test("Should show error after manually navigating to authorize url", async ({
  page,
}) => {
  await page.goto(II_URL + "#authorize");

  // Check for specific error message in II popup
  await expect(
    page.getByRole("heading", { name: "Unsupported Browser" }),
  ).toBeVisible();
});
