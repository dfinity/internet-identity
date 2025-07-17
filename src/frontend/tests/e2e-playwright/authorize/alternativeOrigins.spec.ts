import { test, expect } from "@playwright/test";
import {
  dummyAuth,
  II_URL,
  NOT_TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_URL,
} from "../utils";

// Define canonical URL for testing alternative origins
// const TEST_APP_CANONICAL_URL = "https://canonical-name.ic0.app";

test("Should not issue delegation when alternative origins are empty", async ({
  page,
}) => {
  // Navigate to the test app
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

  // Reset alternative origins
  await page.locator("#hostUrl").fill("https://icp-api.io");
  await page
    .locator("#newAlternativeOrigins")
    .fill('{"alternativeOrigins":[]}');
  await page.locator("#certified").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(
    '{"alternativeOrigins":[]}',
    { timeout: 6000 },
  );

  // Set derivation origin
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Verify error message is displayed in II
  await expect(authPage.getByText("Unverified origin")).toBeVisible();
});

test("Should not issue delegation when origin is missing from /.well-known/ii-alternative-origins", async ({
  page,
}) => {
  // Navigate to the test app
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

  // Reset alternative origins
  await page.locator("#hostUrl").fill("https://icp-api.io");
  const alternativeOrigins = JSON.stringify({
    alternativeOrigins: [NOT_TEST_APP_URL],
  });
  await page.locator("#newAlternativeOrigins").fill(alternativeOrigins);
  await page.locator("#certified").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(
    alternativeOrigins,
    { timeout: 6000 },
  );

  // Set derivation origin
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Verify error message is displayed in II
  await expect(authPage.getByText("Unverified origin")).toBeVisible();
});

// Add a positive test case where alternative origins are properly configured
test("Should issue delegation when derivationOrigin is properly configured in /.well-known/ii-alternative-origins", async ({
  page,
}) => {
  // Navigate to the test app
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  const alternativeOrigins = JSON.stringify({
    alternativeOrigins: [TEST_APP_URL],
  });

  await page.locator("#hostUrl").fill("https://icp-api.io");
  await page.locator("#newAlternativeOrigins").fill(alternativeOrigins);
  await page.locator("#certified").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(
    alternativeOrigins,
    { timeout: 6000 },
  );

  // Set derivation origin
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Create a new identity in II
  const auth = dummyAuth();
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("John Doe");
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

  // Wait for II window to close
  await authPage.waitForEvent("close");

  // Verify successful authentication by checking for a principal
  const principal = await page.locator("#principal").textContent();
  expect(principal).toBeTruthy();
});
