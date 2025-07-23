import { Page, expect } from "@playwright/test";
import { Principal } from "@dfinity/principal";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";

const testAppCanisterId = readCanisterId({ canisterName: "test_app" });
export const II_URL = "https://id.ai";
export const TEST_APP_URL = "https://nice-name.com";
export const NOT_TEST_APP_URL = "https://very-nice-name.com";
export const TEST_APP_CANONICAL_URL = `https://${testAppCanisterId}.icp0.io`;

export type DummyAuthFn = (page: Page) => void;

/**
 * Create dummy auth index value instance
 * @returns Function that can be called to respond to the next index prompt
 */
export const dummyAuth = (): DummyAuthFn => {
  const bytes = crypto.getRandomValues(new Uint8Array(32));
  const hex =
    "0x" +
    Array.from(bytes)
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");
  const index = BigInt(hex);
  return (page: Page) => {
    page.once("dialog", (dialog) => dialog.accept(`${index}`));
  };
};

/**
 * Authorize with test app
 * @param page The page that will load the test app
 * @param authenticate The method that will be called within authorize page
 * @returns Authenticated principal
 */
export const authorize = async (
  page: Page,
  authenticate: (page: Page) => Promise<void>,
): Promise<string> => {
  // Open demo app and assert that user isn't authenticated yet
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
  await expect(page.locator("#principal")).toBeHidden();
  const pagePromise = page.context().waitForEvent("page");

  // Open II window
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Authenticate (with supplied argument fn)
  await authenticate(authPage);

  // Wait for II window to close
  await authPage.waitForEvent("close");

  // Assert that the user is authenticated (valid principal)
  const principal = (await page.locator("#principal").textContent()) ?? "";
  expect(principal).toEqual(Principal.fromText(principal).toText());

  return principal;
};

/**
 * Creates a new identity in II page assuming there is no stored identity
 *
 * @param page
 * @param name
 * @param dummyAuth
 */
export const createNewIdentityInII = async (
  page: Page,
  name: string,
  dummyAuth: DummyAuthFn,
): Promise<void> => {
  // Wait for page to load
  await Promise.any([
    page.getByRole("button", { name: "Continue with Passkey" }).waitFor(),
    page.getByRole("button", { name: "Switch identity" }).waitFor(),
  ]);

  // Check if we're on the continue screen or not
  const onContinueScreen = await page
    .getByRole("button", { name: "Continue with Passkey" })
    .isHidden();
  if (onContinueScreen) {
    // If we're on the continue screen, go through the identity switcher
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Use another identity" }).click();
  }

  // Create passkey identity
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  await page.getByRole("button", { name: "Set up a new Passkey" }).click();
  await page.getByLabel("Identity name").fill(name);
  dummyAuth(page);
  await page.getByRole("button", { name: "Create Passkey" }).click();

  if (onContinueScreen) {
    // If we're coming from the continue screen (through identity switcher),
    // we'll also need to explicitly select the primary account to continue.
    await page.getByRole("button", { name: "Primary account" }).click();
  }
};

/**
 * Create passkey identity with dummy auth
 * @param page The authorization page (either initial or continue)
 * @param name The name that should be given to the identity
 * @param dummyAuth The dummy auth instance to create the identity with
 */
export const createIdentity = (
  page: Page,
  name: string,
  dummyAuth: DummyAuthFn,
): Promise<string> =>
  authorize(page, async (authPage) => {
    await createNewIdentityInII(authPage, name, dummyAuth);
  });

/**
 * Clear II localstorage, this is particularly used in tests
 * where the last used data need to be cleared to make sure
 * that we're back at the initial non-continue page.
 */
export const clearStorage = async (page: Page): Promise<void> => {
  await page.goto(II_URL);
  await page.evaluate(() => localStorage.clear());
};

export const addPasskeyCurrentDevice = async (
  page: Page,
  dummyAuth: DummyAuthFn,
): Promise<void> => {
  await page.getByRole("button", { name: "Add" }).click();
  await page.getByRole("button", { name: "Continue with Passkey" }).click();
  dummyAuth(page);
  await page.getByRole("button", { name: "Create Passkey" }).click();
};

export const renamePasskey = async (
  page: Page,
  name: string,
): Promise<void> => {
  await expect(page.getByLabel("Rename passkey")).toHaveCount(1);
  await page.getByLabel("Rename passkey").click();

  // Wait for the rename dialog to open
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeVisible();

  const input = page.getByRole("textbox");
  await input.clear();
  await input.fill(name);
  await page.getByRole("button", { name: "Save" }).click();
};

export const signOut = async (page: Page): Promise<void> => {
  await page.getByLabel("Switch identity").click();
  await page.getByRole("button", { name: "Sign Out" }).click();
};
