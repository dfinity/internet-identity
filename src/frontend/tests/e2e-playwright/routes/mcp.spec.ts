import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";

/** A target app passed in the request. It is ignored by the connect flow (the
 *  delegation acts as the user's account at the configured MCP-server origin),
 *  but kept to exercise that the param is tolerated. */
const APP = "nice-name.com";

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

// The browser /mcp flow issues the standing credential for the user's account
// at the configured MCP *server* origin (not a per-request app); per-app
// delegations are minted server-side by the `mcp_prepare/get_account_delegation`
// canister methods. Principal-derivation parity for those is canister logic and
// is covered by the integration tests (tests/integration/mcp.rs), not here.
