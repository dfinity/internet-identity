import { expect, test } from "@playwright/test";
import { dummyAuth, II_URL, TEST_APP_URL } from "../utils";

test("Delegation maxTimeToLive: 1 min", async ({ page }) => {
  const auth = dummyAuth();

  // Open demo app and configure II URL
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

  // Set maxTimeToLive to 1 minute (60 seconds = 60_000_000_000 nanoseconds)
  await page.locator("#maxTimeToLive").fill("60000000000");

  // Start authentication flow
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Create new identity and authenticate
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("Test User");
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

  // Wait for authentication to complete and window to close
  await authPage.waitForEvent("close");

  // Wait for principal to be populated (indicating successful authentication)
  await expect(page.locator("#principal")).not.toBeEmpty();

  // Check expiration time - should be close to 1 minute (60_000_000_000 nanoseconds)
  const expiration = await page.locator("#expiration").textContent();
  const expirationNs = Number(expiration);
  // Compare only up to one decimal place for the 1min test
  expect(expirationNs / 60_000_000_000).toBeCloseTo(1, 0);
});

test("Delegation maxTimeToLive: 1 day", async ({ page }) => {
  const auth = dummyAuth();

  // Open demo app and configure II URL
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

  // Set maxTimeToLive to 1 day (86400 seconds = 86400_000_000_000 nanoseconds)
  await page.locator("#maxTimeToLive").fill("86400000000000");

  // Start authentication flow
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Create new identity and authenticate
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("Test User");
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

  // Wait for authentication to complete and window to close
  await authPage.waitForEvent("close");

  // Wait for principal to be populated (indicating successful authentication)
  await expect(page.locator("#principal")).not.toBeEmpty();

  // Check expiration time - should be close to 1 day (86400_000_000_000 nanoseconds)
  const expiration = await page.locator("#expiration").textContent();
  const expirationNs = Number(expiration);
  expect(expirationNs / 86400_000_000_000).toBeCloseTo(1);
});

test("Delegation maxTimeToLive: 2 months", async ({ page }) => {
  const auth = dummyAuth();

  // Open demo app and configure II URL
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);

  // Set maxTimeToLive to 60 days (5_184_000_000_000_000 nanoseconds)
  await page.locator("#maxTimeToLive").fill("5184000000000000");

  // Start authentication flow
  const pagePromise = page.context().waitForEvent("page");
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Create new identity and authenticate
  await authPage.getByRole("button", { name: "Continue with Passkey" }).click();
  await authPage.getByRole("button", { name: "Set up a new Passkey" }).click();
  await authPage.getByLabel("Identity name").fill("Test User");
  auth(authPage);
  await authPage.getByRole("button", { name: "Create Passkey" }).click();

  // Wait for authentication to complete and window to close
  await authPage.waitForEvent("close");

  // Wait for principal to be populated (indicating successful authentication)
  await expect(page.locator("#principal")).not.toBeEmpty();

  // Check expiration time - should be capped at 30 days (2_592_000_000_000_000 nanoseconds)
  const expiration = await page.locator("#expiration").textContent();
  const expirationNs = Number(expiration);
  // NB: Max out at 30 days
  expect(expirationNs / 2_592_000_000_000_000).toBeCloseTo(1);
});
