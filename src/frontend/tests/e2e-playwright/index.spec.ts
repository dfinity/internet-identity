import { test } from "@playwright/test";
import { clearStorage, createIdentity, dummyAuth, II_URL } from "./utils";

test.describe("First visit", () => {
  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill("John Doe");
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
  });

  test("Sign in with an existing passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, "John Doe", auth);
    await clearStorage(page);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
  });
});

test.describe("Last used identities listed", () => {
  test("Sign in with last used identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, "John Doe", auth);
    await page.goto(II_URL);
    auth(page);
    await page.getByRole("button", { name: "John Doe" }).click();
    await page.waitForURL(II_URL + "/manage");
  });

  test("Sign in with another identity", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, "John Doe", auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    auth(page);
    await page.getByRole("button", { name: "Use an existing Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
  });

  test("Sign up with a new passkey", async ({ page }) => {
    const auth = dummyAuth();
    await createIdentity(page, "John Doe", auth);
    await page.goto(II_URL);
    await page.getByRole("button", { name: "Use another identity" }).click();
    await page.getByRole("button", { name: "Continue with Passkey" }).click();
    await page.getByRole("button", { name: "Set up a new Passkey" }).click();
    await page.getByLabel("Identity name").fill("Jane Doe");
    auth(page);
    await page.getByRole("button", { name: "Create Passkey" }).click();
    await page.waitForURL(II_URL + "/manage");
  });
});
