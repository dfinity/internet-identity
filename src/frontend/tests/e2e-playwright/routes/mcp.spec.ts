import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";

/** A target app passed in the request. It is ignored by the connect flow (the
 *  delegation acts as the user's chosen account at the MCP-server origin the
 *  request's callback identifies), but kept to exercise that the param is
 *  tolerated. */
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

test("Invalid params show the error screen", async ({ page }) => {
  await page.goto(II_URL + "/mcp");
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("A non-https, non-loopback callback is rejected", async ({
  page,
  mcp,
}) => {
  // Each user connects whichever MCP server they trust, so any https origin (or
  // http loopback for a server they run themselves) is accepted. A plain http
  // origin on a remote host is not — the /mcp `form-action` CSP wouldn't allow
  // posting the delegation there, so the flow rejects it up front.
  await page.goto(
    mcp.buildAuthorizeUrl({
      app: APP,
      callbackUrl: "http://evil.example.com/cb",
    }),
  );
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("Signing up to an untrusted server prompts to add it in settings", async ({
  page,
  mcp,
}) => {
  // Each identity trusts no MCP server by default, and a fresh in-flow sign-up
  // has no chance to pre-trust one, so it lands on the untrusted screen rather
  // than connecting.
  await addVirtualAuthenticator(page);
  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await signUp(page);
  await expect(
    page.getByRole("heading", { name: "This MCP server isn't trusted yet" }),
  ).toBeVisible();
  await expect(
    page.getByRole("link", { name: "Manage trusted server" }),
  ).toBeVisible();
});

test("After trusting the server, the connect screen shows", async ({
  page,
  mcp,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await expect(
    page.getByRole("button", { name: "Allow access" }),
  ).toBeVisible();
});

test("Adding a trusted server in Settings unlocks the connect screen", async ({
  page,
  mcp,
}) => {
  // End-to-end: add the server via the Settings UI (not the seeding helper),
  // then connecting to it reaches the connect screen.
  await addVirtualAuthenticator(page);
  // The Settings UI verifies the server with a real MCP `initialize` handshake
  // (a cross-origin POST). Answer the CORS preflight and reply with a JSON-RPC
  // initialize result + CORS headers so the browser lets the probe read it.
  // (Activation is independent of the probe outcome, so the assertions below
  // hold whether or not the browser surfaces it as verified.)
  const cors = {
    "access-control-allow-origin": "*",
    "access-control-allow-methods": "POST, OPTIONS",
    "access-control-allow-headers": "content-type",
  };
  await page.route(`${mcp.mcpOrigin}/**`, (route) => {
    if (route.request().method() === "OPTIONS") {
      return route.fulfill({ status: 204, headers: cors });
    }
    return route.fulfill({
      status: 200,
      headers: { ...cors, "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        result: {
          protocolVersion: "2025-06-18",
          capabilities: {},
          serverInfo: { name: "test-mcp", version: "1" },
        },
      }),
    });
  });
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  // Reach Settings via in-app navigation rather than page.goto: a full reload
  // of an /manage/(authenticated) route drops the just-signed-up in-memory
  // session and lands on the sign-in screen, whereas the SPA nav keeps it. On
  // mobile the sidebar is collapsed behind a menu button, so open it first.
  const openMenu = page.getByRole("button", { name: "Open menu" });
  if (await openMenu.isVisible()) {
    await openMenu.click();
  }
  await page.locator('a[href="/manage/settings"]').click();
  await page.waitForURL(II_URL + "/manage/settings");
  await page.getByLabel("MCP server URL").fill(`${mcp.mcpOrigin}/mcp`);
  await page.getByRole("button", { name: "Add" }).click();
  // The trusted server is shown with a remove button whose aria-label carries
  // the full URL — a unique assertion that it was added.
  await expect(
    page.getByRole("button", { name: `Remove ${mcp.mcpOrigin}/mcp` }),
  ).toBeVisible();

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await expect(
    page.getByRole("button", { name: "Allow access" }),
  ).toBeVisible();
});

test("Returning user lands on the connect screen immediately", async ({
  page,
  mcp,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await expect(
    page.getByRole("button", { name: "Allow access" }),
  ).toBeVisible();
});

test("Allow access posts a two-hop delegation chain", async ({ page, mcp }) => {
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

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
}) => {
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

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

test("Requested TTL within bounds is honoured", async ({ page, mcp }) => {
  const ttlMinutes = 60;
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  const before = Date.now();
  await page.goto(mcp.buildAuthorizeUrl({ app: APP, ttlMinutes }));
  await page.getByRole("button", { name: "Allow access" }).click();

  const expMillis = expirationMillis(await mcp.receivedDelegation);
  const requestedMillis = ttlMinutes * 60 * 1000;
  expect(expMillis - before).toBeGreaterThanOrEqual(requestedMillis - 60_000);
  expect(expMillis - before).toBeLessThanOrEqual(requestedMillis + 60_000);
});

// The browser /mcp flow issues the standing credential for the user's chosen
// account at the MCP *server* origin the callback identifies (not a per-request
// app); per-app delegations are minted server-side by the
// `mcp_prepare/get_account_delegation` canister methods. Principal-derivation
// parity for those is canister logic, covered by the integration tests
// (tests/integration/mcp.rs), not here.
