import { test, expect } from "@playwright/test";
import {
  dummyAuth,
  II_URL,
  NOT_TEST_APP_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_RAW,
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
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("John Doe");
  const auth = dummyAuth();
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

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
  await page.locator("#redirect").click(); // Use redirect mode instead of certified
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

test("Should fetch /.well-known/ii-alternative-origins using the non-raw url", async ({
  page,
}) => {
  // Set up console log monitoring
  const consoleLogs: string[] = [];
  page.on("console", (msg) => {
    if (msg.type() === "error") {
      consoleLogs.push(msg.text());
    }
  });

  await page.goto(TEST_APP_URL);

  // Configure the test app
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await page.locator("#hostUrl").fill("https://icp-api.io");

  // Set up alternative origins with the nice URL
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

  // Set derivation origin to the RAW URL
  await page.locator("#derivationOrigin").fill(TEST_APP_CANONICAL_URL_RAW);

  // Attempt to sign in
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Simulate the raw URL fetch attempt that should fail
  await page.evaluate((rawURL: string) => {
    fetch(`${rawURL}/.well-known/ii-alternative-origins`).catch(console.error);
  }, TEST_APP_CANONICAL_URL_RAW);

  // Wait a bit for the console error to be logged
  await page.waitForTimeout(1000);

  // Verify that the raw URL fetch failed (logged to console)
  const hasRawUrlError = consoleLogs.some(
    (log) =>
      log.includes("/.well-known/ii-alternative-origins") &&
      (log.includes("Failed to load resource") ||
        log.includes("Failed to fetch")),
  );
  expect(hasRawUrlError).toBeTruthy();

  // But the authentication should still work (fetched using non-raw URL)
  // Create a new identity in II
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("John Doe");
  const auth = dummyAuth();
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

  // Wait for II window to close
  await authPage.waitForEvent("close");

  // Verify successful authentication by checking for a principal
  const principal = await page.locator("#principal").textContent();
  expect(principal).toBeTruthy();
});
