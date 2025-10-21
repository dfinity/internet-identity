import { test, expect } from "@playwright/test";
import {
  dummyAuth,
  II_URL,
  NOT_TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_URL,
} from "../utils";

test("Should not issue delegation when alternative origins are empty", async ({
  page,
}) => {
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
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
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
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
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Create new identity" }).click();
  await authPage.getByLabel("Identity name").fill("John Doe");
  const auth = dummyAuth();
  auth(authPage);
  await authPage.getByRole("button", { name: "Create identity" }).click();
  await authPage.getByRole("button", { name: "Continue", exact: true }).click();

  // Wait for II window to close
  await authPage.waitForEvent("close");

  // Verify successful authentication by checking for a principal
  const principal = await page.locator("#principal").textContent();
  expect(principal).toBeTruthy();
});

test("Should not issue delegation when /.well-known/ii-alternative-origins has too many entries", async ({
  page,
}) => {
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await page.locator("#hostUrl").fill("https://icp-api.io");

  // Set up alternative origins with 11 entries (exceeding the 10 limit)
  const tooManyOrigins = JSON.stringify({
    alternativeOrigins: [
      "https://a0.com",
      "https://a1.com",
      "https://a2.com",
      "https://a3.com",
      "https://a4.com",
      "https://a5.com",
      "https://a6.com",
      "https://a7.com",
      "https://a8.com",
      "https://a9.com",
      "https://a10.com",
    ],
  });

  await page.locator("#newAlternativeOrigins").fill(tooManyOrigins);
  await page.locator("#certified").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(tooManyOrigins, {
    timeout: 6000,
  });

  // Set derivation origin
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Verify error message is displayed in II
  await expect(authPage.getByText("Unverified origin")).toBeVisible();
});

test("Should not follow redirect returned by /.well-known/ii-alternative-origins", async ({
  page,
}) => {
  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await page.locator("#hostUrl").fill("https://icp-api.io");

  // Set up alternative origins that will return a redirect
  const redirectOrigins = JSON.stringify({
    alternativeOrigins: ["https://evil.com"],
  });

  await page.locator("#newAlternativeOrigins").fill(redirectOrigins);
  // Enable redirect mode to configure the test app to simulate a scenario where
  // alternative origins return a redirect. This is necessary to verify that the
  // system does not follow unverified redirects, ensuring security.
  await page.locator("#redirect").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(
    redirectOrigins,
    { timeout: 6000 },
  );

  // Set derivation origin
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Verify error message is displayed in II (redirect should not be followed)
  await expect(authPage.getByText("Unverified origin")).toBeVisible();
});

test("Should issue the same principal to nice url and canonical url", async ({
  page,
}) => {
  // Create a new identity using dummy auth
  const auth = dummyAuth();

  // First authentication: Test app configured with canonical URL as derivation origin
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await page.locator("#hostUrl").fill("https://icp-api.io");

  // Configure alternative origins to include the nice URL
  const alternativeOrigins = JSON.stringify({
    alternativeOrigins: [TEST_APP_URL],
  });
  await page.locator("#newAlternativeOrigins").fill(alternativeOrigins);
  await page.locator("#certified").click();
  await page.locator("#updateNewAlternativeOrigins").click();

  // Wait for alternative origins to update
  await expect(page.locator("#alternativeOrigins")).toHaveText(
    alternativeOrigins,
    { timeout: 6000 },
  );

  // Set derivation origin to canonical URL
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL);

  // Authenticate and get the first principal
  const pagePromise1 = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage1 = await pagePromise1;

  // Create identity in II
  await authPage1
    .getByRole("button", { name: "Continue with Passkey" })
    .click();
  await authPage1.getByRole("button", { name: "Create new identity" }).click();
  await authPage1.getByLabel("Identity name").fill("Test User");
  auth(authPage1);
  await authPage1.getByRole("button", { name: "Create identity" }).click();
  await authPage1
    .getByRole("button", { name: "Continue", exact: true })
    .click();

  // Wait for II window to close
  await authPage1.waitForEvent("close");

  // Get the first principal
  await expect(page.locator("#principal")).not.toBeEmpty();
  const principal1 = await page.locator("#principal").textContent();
  expect(principal1).toBeTruthy();

  // Second authentication: Simulate nice URL scenario with same derivation origin
  // Clear the current session by reloading the page
  await page.reload();
  await page.goto(TEST_APP_CANONICAL_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await page.locator("#hostUrl").fill("https://icp-api.io");

  // Authenticate with the existing identity
  const pagePromise2 = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage2 = await pagePromise2;

  // Use existing passkey
  auth(authPage2);
  await authPage2
    .getByRole("button", { name: "Continue", exact: true })
    .click();

  // Wait for II window to close
  await authPage2.waitForEvent("close");

  // Get the second principal
  await expect(page.locator("#principal")).not.toBeEmpty();
  const principal2 = await page.locator("#principal").textContent();
  expect(principal2).toBeTruthy();

  // Verify both principals are the same
  expect(principal1).toEqual(principal2);
});
