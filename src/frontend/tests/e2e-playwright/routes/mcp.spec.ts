import { expect, type Page } from "@playwright/test";
import { Principal } from "@icp-sdk/core/principal";
import { test } from "../fixtures";
import { addVirtualAuthenticator, authorize, II_URL } from "../utils";

/** The app the MCP delegation acts as, used across the tests. */
const APP = "nice-name.com";

/** Decodes a hex string (as delegation chains encode public keys) to bytes. */
const hexToBytes = (hex: string): Uint8Array =>
  Uint8Array.from(hex.match(/.{1,2}/g) ?? [], (byte) => parseInt(byte, 16));

const signUp = async (page: Page): Promise<void> => {
  const continueWithPasskey = page.getByRole("button", {
    name: "Continue with passkey",
  });
  const signUpToggle = page.getByRole("button", {
    name: "Create",
    exact: true,
  });
  await continueWithPasskey.or(signUpToggle).first().waitFor();
  if (await continueWithPasskey.isVisible()) {
    await continueWithPasskey.click();
    await page.getByRole("button", { name: "Create new identity" }).click();
  } else {
    await signUpToggle.click();
    await page.getByRole("button", { name: "Create with passkey" }).click();
  }
  await page.getByLabel("Identity name").fill("Test User");
  await page.getByRole("button", { name: "Create identity" }).click();
};

/** Returns the chain's first-delegation expiration in milliseconds since epoch. */
const expirationMillis = (body: unknown): number => {
  if (
    typeof body !== "object" ||
    body === null ||
    !("delegations" in body) ||
    !Array.isArray(body.delegations) ||
    body.delegations.length === 0
  ) {
    throw new Error("delegation chain missing or empty");
  }
  const expiration: unknown = body.delegations[0]?.delegation?.expiration;
  if (typeof expiration !== "string") {
    throw new Error("delegation expiration missing");
  }
  return Number(BigInt(`0x${expiration}`) / BigInt(1_000_000));
};

/** The hex root public key of a delegation chain (the per-account key). */
const rootPublicKey = (body: unknown): string => {
  if (
    typeof body === "object" &&
    body !== null &&
    "publicKey" in body &&
    typeof body.publicKey === "string"
  ) {
    return body.publicKey;
  }
  throw new Error("delegation chain missing publicKey");
};

/**
 * Enables device-local MCP access for the signed-in identity via Settings.
 * Assumes the page is already on `/manage`.
 */
const enableMcpAccessInSettings = async (
  page: Page,
  isMobile: boolean,
): Promise<void> => {
  if (isMobile) {
    await page.getByRole("button", { name: "Open menu" }).click();
  }
  await page.getByRole("link", { name: "Settings" }).click();
  await page.waitForURL(II_URL + "/manage/settings");
  await page.getByRole("switch", { name: "MCP access" }).check();
  await page.getByLabel("I understand the risks.").check();
  await page.getByRole("button", { name: "Enable MCP access" }).click();
  await expect(page.getByText("Enabled", { exact: true })).toBeVisible();
};

test("Invalid params show the error screen", async ({ page }) => {
  await page.goto(II_URL + "/mcp");
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("A callback off the configured MCP origin is rejected", async ({
  page,
  mcp,
}) => {
  await page.goto(
    mcp.buildAuthorizeUrl({
      app: APP,
      callbackUrl: "https://attacker.example.com/cb",
    }),
  );
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("Without MCP access enabled the gated screen shows", async ({
  page,
  mcp,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await signUp(page);
  await expect(
    page.getByRole("heading", { name: "MCP access not enabled" }),
  ).toBeVisible();
});

test("Returning user without MCP access lands on the gated screen immediately", async ({
  page,
  mcp,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await expect(
    page.getByRole("heading", { name: "MCP access not enabled" }),
  ).toBeVisible();
  await expect(page.getByRole("button", { name: "Allow access" })).toBeHidden();
});

test("Once MCP access is enabled, Allow access posts a two-hop delegation chain", async ({
  page,
  mcp,
  isMobile,
}) => {
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await enableMcpAccessInSettings(page, isMobile);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await page.getByRole("button", { name: "Allow access" }).click();

  const body = await mcp.receivedDelegation;
  expect(body).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  // Rooted at the user's principal: canister-signed delegation to the ephemeral
  // browser key, then the ephemeral key's sub-delegation to the MCP server's
  // public key.
  if (
    typeof body === "object" &&
    body !== null &&
    "delegations" in body &&
    Array.isArray(body.delegations)
  ) {
    expect(body.delegations.length).toBe(2);
  }
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});

test("Identity switcher shows while signing in and hides on the success screen", async ({
  page,
  mcp,
  isMobile,
}) => {
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await enableMcpAccessInSettings(page, isMobile);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  const switcher = page.getByRole("button", { name: "Switch identity" });
  const allow = page.getByRole("button", { name: "Allow access" });
  await expect(allow).toBeVisible();
  await expect(switcher).toBeVisible();

  await allow.click();
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
  await expect(switcher).toBeHidden();
});

test("Requested TTL within bounds is honoured", async ({
  page,
  mcp,
  isMobile,
}) => {
  const ttlMinutes = 60;
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await enableMcpAccessInSettings(page, isMobile);

  const before = Date.now();
  await page.goto(mcp.buildAuthorizeUrl({ app: APP, ttlMinutes }));
  await page.getByRole("button", { name: "Allow access" }).click();

  const expMillis = expirationMillis(await mcp.receivedDelegation);
  const requestedMillis = ttlMinutes * 60 * 1000;
  expect(expMillis - before).toBeGreaterThanOrEqual(requestedMillis - 60_000);
  expect(expMillis - before).toBeLessThanOrEqual(requestedMillis + 60_000);
});

test("MCP acts as the same principal that /authorize gives for that app", async ({
  page,
  mcp,
  identities,
  signInWithIdentity,
  isMobile,
}) => {
  const identityNumber = identities[0].identityNumber;

  // Principal the app (nice-name.com) sees via the normal /authorize flow — its
  // default account.
  const authorizePrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });

  // Enable device MCP access for the same identity.
  await page.goto(II_URL);
  await signInWithIdentity(page, identityNumber);
  await page.waitForURL(II_URL + "/manage");
  await enableMcpAccessInSettings(page, isMobile);

  // Principal the MCP server is granted: the self-authenticating principal of
  // the delegation chain's root public key.
  await mcp.installInterceptor(page);
  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await page.getByRole("button", { name: "Allow access" }).click();
  const chain = await mcp.receivedDelegation;
  const mcpPrincipal = Principal.selfAuthenticating(
    hexToBytes(rootPublicKey(chain)),
  ).toText();

  // Same identity + same default account for the app ⇒ same principal.
  expect(mcpPrincipal).toBe(authorizePrincipal);
});
