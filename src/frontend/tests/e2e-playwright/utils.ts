import { CDPSession, expect, Page } from "@playwright/test";
import { Principal } from "@icp-sdk/core/principal";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";
import Protocol from "devtools-protocol";
import { isNullish } from "@dfinity/utils";

const testAppCanisterId = readCanisterId({ canisterName: "test_app" });
export const II_URL = "https://id.ai";
export const LEGACY_II_URL = "https://identity.ic0.app";
export const TEST_APP_URL = "https://nice-name.com";
export const NOT_TEST_APP_URL = "https://very-nice-name.com";
export const TEST_APP_CANONICAL_URL = `https://${testAppCanisterId}.icp0.io`;

export type DummyAuthFn = (page: Page) => void;

export const getRandomIndex = (): bigint => {
  const bytes = crypto.getRandomValues(new Uint8Array(32));
  const hex =
    "0x" +
    Array.from(bytes)
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");
  return BigInt(hex);
};

/**
 * Create dummy auth index value instance
 * @returns Function that can be called to respond to the next index prompt
 */
export const dummyAuth = (index = getRandomIndex()): DummyAuthFn => {
  return (page: Page) => {
    page.once("dialog", (dialog) => dialog.accept(`${index}`));
  };
};

export const cancelDummyAuth = (page: Page) => {
  page.once("dialog", (dialog) => dialog.dismiss());
};

/**
 * Authorize with test app
 * @param page The page that will load the test app
 * @param authenticate The method that will be called within authorize page
 * @returns Authenticated principal
 */
export const authorize = (
  page: Page,
  authenticate: (page: Page) => Promise<void>,
): Promise<string> => {
  return authorizeWithUrl(page, TEST_APP_URL, II_URL, authenticate);
};

/**
 * Authorize with a custom app URL
 * @param page The page that will load the test app
 * @param appUrl The URL of the app to authorize with
 * @param authenticate The method that will be called within authorize page
 * @returns Authenticated principal
 */
export const authorizeWithUrl = async (
  page: Page,
  appUrl: string,
  iiURL: string,
  authenticate: (page: Page) => Promise<void>,
  useIcrc25?: boolean,
  attributes?: string[],
): Promise<string> => {
  // Open demo app and assert that user isn't authenticated yet
  await page.goto(appUrl);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(iiURL);
  if (useIcrc25 === true) {
    await page
      .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
      .setChecked(true);

    // If also requesting attributes, fill them in the textarea
    if (attributes !== undefined) {
      await page
        .getByRole("textbox", { name: "Request attributes:" })
        .fill(attributes.join("\n"));
    }
  }
  await expect(page.locator("#principal")).toBeHidden();
  const pagePromise = page.context().waitForEvent("page");

  // Open II window
  await page.getByRole("button", { name: "Sign In" }).click();
  const authPage = await pagePromise;

  // Authenticate (with supplied argument fn)
  await authenticate(authPage);

  // Wait for II window to close.
  // During the identity upgrade flow there is a delay of 5s
  await authPage.waitForEvent("close", { timeout: 15_000 });

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
    page.getByRole("button", { name: "Continue with passkey" }).waitFor(),
    page.getByRole("button", { name: "Switch identity" }).waitFor(),
  ]);

  // Check if we're on the continue screen or not
  const onContinueScreen = await page
    .getByRole("button", { name: "Continue with passkey" })
    .isHidden();
  if (onContinueScreen) {
    // If we're on the continue screen, go through the identity switcher
    await page.getByRole("button", { name: "Switch identity" }).click();
    await page.getByRole("button", { name: "Use another identity" }).click();
  }

  // Create passkey identity
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  await page.getByRole("button", { name: "Create new identity" }).click();
  await page.getByLabel("Identity name").fill(name);
  dummyAuth(page);
  await page.getByRole("button", { name: "Create identity" }).click();
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

    // Continue to dapp
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
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
  await page.getByRole("button", { name: "Add new" }).click();
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  dummyAuth(page);
  await page.getByRole("button", { name: "Create Passkey" }).click();
};

export const renamePasskey = async (
  page: Page,
  currentName: string,
  nextName: string,
): Promise<void> => {
  // Open the rename passkey dialog
  await page
    .getByRole("listitem")
    .filter({ hasText: "Passkey" })
    .filter({ hasText: currentName })
    .getByRole("button", { name: "More options" })
    .click();
  await page.getByRole("menuitem", { name: "Rename" }).click();

  // Wait for the rename dialog to open
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeVisible();

  const input = page.getByRole("textbox");
  await input.clear();
  await input.fill(nextName);
  await page.getByRole("button", { name: "Save changes" }).click();

  // Wait for the rename dialog to close
  await expect(
    page.getByRole("heading", { name: "Rename passkey" }),
  ).toBeHidden();
};

export const removePasskey = async (
  page: Page,
  name: string,
  isCurrent: boolean,
): Promise<void> => {
  // Open the remove passkey dialog
  await page
    .getByRole("listitem")
    .filter({ hasText: name })
    .getByRole("button", { name: "More options" })
    .click();
  await page.getByRole("menuitem", { name: "Remove" }).click();

  // Wait for the remove dialog to open with the correct message
  await expect(
    page.getByRole("heading", { name: "Are you sure?" }),
  ).toBeVisible();
  await expect(
    page.getByText(
      "Removing this passkey means you won't be able to use it to sign in anymore. You can always add a new one later.",
    ),
  ).toBeVisible();
  const signedInMessage = page.getByText(
    "As you are currently signed in with this passkey, you will be signed out.",
  );
  if (isCurrent) {
    await expect(signedInMessage).toBeVisible();
  } else {
    await expect(signedInMessage).toBeHidden();
  }

  // Remove the passkey
  await page.getByRole("button", { name: "Remove passkey" }).click();

  // Wait for the remove dialog to close
  await expect(
    page.getByRole("heading", { name: "Are you sure?" }),
  ).toBeHidden();
};

export const signOut = async (page: Page): Promise<void> => {
  // Navigating to landing page (reloading) is sufficient for now to sign out
  await page.goto(II_URL);
};

/**
 * Opens test app and configures II URL
 */
export const openTestAppWithII = async (page: Page): Promise<void> => {
  await page.goto(TEST_APP_URL);
  await page.getByRole("textbox", { name: "Identity Provider" }).fill(II_URL);
};

/**
 * Opens II popup tab and returns the popup page
 */
export const openIiTab = async (page: Page): Promise<Page> => {
  const pagePromise = page.context().waitForEvent("page");
  await page.locator("#openIiWindowBtn").click();
  return await pagePromise;
};

/**
 * Waits for nth message to appear in test app
 */
export const waitForNthMessage = async (
  page: Page,
  messageNo: number,
): Promise<void> => {
  await page.locator(`div.postMessage:nth-child(${messageNo})`).waitFor();
};

/**
 * Gets message text from nth message in test app
 */
export const getMessageText = async (
  page: Page,
  messageNo: number,
): Promise<string> => {
  return (
    (await page
      .locator(`div.postMessage:nth-child(${messageNo}) > div:nth-child(2)`)
      .textContent()) ?? ""
  );
};

// WebAuthn CDP client cache to reuse the same session per Page
const webauthnClientCache = new WeakMap<Page, Promise<CDPSession>>();
const getWebAuthnClient = (page: Page) => {
  let clientPromise = webauthnClientCache.get(page);
  if (isNullish(clientPromise)) {
    clientPromise = (async () => {
      const client = await page.context().newCDPSession(page);
      await client.send("WebAuthn.enable");
      return client;
    })();
    webauthnClientCache.set(page, clientPromise);
  }
  return clientPromise;
};

/**
 * Adds a virtual authenticator to the browser
 * @param page The page to add the virtual authenticator to
 * @returns The authenticator ID
 */
export const addVirtualAuthenticator = async (page: Page): Promise<string> => {
  const client = await getWebAuthnClient(page);
  const { authenticatorId } = await client.send(
    "WebAuthn.addVirtualAuthenticator",
    {
      options: {
        protocol: "ctap2",
        transport: "usb",
        hasResidentKey: true,
        hasUserVerification: true,
        isUserVerified: true,
      },
    },
  );
  return authenticatorId;
};

/**
 * Gets credentials from a virtual authenticator
 * @param page The page to get the credentials from
 * @param authenticatorId The authenticator ID
 * @returns The credentials
 */
export const getCredentialsFromVirtualAuthenticator = async (
  page: Page,
  authenticatorId: string,
): Promise<Protocol.WebAuthn.Credential[]> => {
  const client = await getWebAuthnClient(page);
  const { credentials } = await client.send("WebAuthn.getCredentials", {
    authenticatorId,
  });
  return credentials;
};

/**
 * Adds a credential to a virtual authenticator
 * @param page The page to add the credential to
 * @param authenticatorId The authenticator ID
 * @param credential The credential to add
 */
export const addCredentialToVirtualAuthenticator = async (
  page: Page,
  authenticatorId: string,
  credential: Protocol.WebAuthn.Credential,
): Promise<void> => {
  const client = await getWebAuthnClient(page);
  await client.send("WebAuthn.addCredential", {
    authenticatorId,
    credential,
  });
};

/**
 * Read current text value from clipboard, this is an alternative to
 * `navigator.clipboard.readText()` since that is blocked by CSP.
 */
export const readClipboard = async (page: Page): Promise<string> => {
  // Create temporary text area and focus it
  const textarea = await page.evaluateHandle(() => {
    const el = document.createElement("textarea");
    // Append element to dialog if present so it can always be focused
    const dialog = document.body.querySelector("dialog");
    (dialog ?? document.body).appendChild(el);
    el.focus();
    return el;
  });
  // Paste clipboard content into it, read the textarea value and clean it up
  await page.keyboard.press(
    process.platform === "darwin" ? "Meta+V" : "Control+V",
  );
  return page.evaluate((el) => {
    const text = el.value;
    el.remove();
    return text;
  }, textarea);
};
