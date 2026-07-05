import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";

/** A target app passed in the request. It is ignored by the connect flow (the
 *  session is registered for the user's identity; the app account is chosen
 *  server-side per call), but kept to exercise that the param is tolerated. */
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

/** Converts the completion's grant expiration (ns since epoch, decimal string)
 *  to milliseconds since epoch. */
const expirationMillis = (expiration: string): number =>
  Number(BigInt(expiration) / BigInt(1_000_000));

test("Invalid params show the error screen", async ({ page }) => {
  await page.goto(II_URL + "/mcp");
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("A non-https callback is rejected", async ({ page, mcp }) => {
  // MCP connections are to remote servers only, so callbacks must be https. A
  // plain-http (or loopback) origin is rejected up front, before the connect
  // flow would talk to it.
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
  // A fresh sign-up trusts no MCP server yet. The connect screen shows
  // optimistically, but at connect time — after the user authenticates — II
  // verifies the server against the identity's synced config and, finding it
  // untrusted, shows the screen pointing to Settings rather than connecting.
  await addVirtualAuthenticator(page);
  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await signUp(page);
  await page.getByRole("button", { name: "Allow access" }).click();
  await expect(
    page.getByRole("heading", { name: "This MCP server isn't trusted yet" }),
  ).toBeVisible();
  // The button authenticates here and hands the session to a new Settings tab,
  // rather than opening Settings cold (which would force a fresh sign-in).
  await expect(
    page.getByRole("button", { name: "Manage trusted server" }),
  ).toBeVisible();
});

test("Manage trusted server hands the session to a new Settings tab", async ({
  page,
  mcp,
}) => {
  // Reaching the untrusted screen authenticates the identity, so its "Manage
  // trusted server" button can open Settings in a new tab that adopts the
  // session via postMessage — the same handoff as the authorize header, so no
  // second sign-in is needed over there.
  await addVirtualAuthenticator(page);
  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await signUp(page);
  await page.getByRole("button", { name: "Allow access" }).click();
  const manageButton = page.getByRole("button", {
    name: "Manage trusted server",
  });
  await expect(manageButton).toBeVisible();

  const settingsPagePromise = page.context().waitForEvent("page");
  await manageButton.click();
  const settingsPage = await settingsPagePromise;
  await settingsPage.waitForURL("**/manage/settings**", { timeout: 15_000 });

  // The handed-off session means Settings opens authenticated: the trusted
  // server section heading is shown and no sign-in screen appears.
  await expect(
    settingsPage.getByRole("heading", { name: "Trusted MCP server" }),
  ).toBeVisible({ timeout: 10_000 });
});

test("After trusting the server, the connect screen shows", async ({
  page,
  mcp,
}) => {
  // Trust now lives on-chain (synced), so trustServer drives the Settings UI and
  // the connect flow makes real canister round-trips — give it ample time.
  test.slow();
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
  test.slow();
  await addVirtualAuthenticator(page);
  // The Settings UI verifies the server via its RFC 9728 protected-resource
  // metadata (a CORS-enabled GET at /.well-known/oauth-protected-resource).
  // Serve a valid doc with CORS headers so the probe confirms MCP. (Activation
  // is independent of the probe outcome, so the assertions below hold either
  // way.) The catch-all also answers the `initialize` fallback + its preflight.
  const cors = {
    "access-control-allow-origin": "*",
    "access-control-allow-methods": "GET, POST, OPTIONS",
    "access-control-allow-headers": "content-type",
  };
  await page.route(`${mcp.mcpOrigin}/**`, (route) => {
    if (route.request().method() === "OPTIONS") {
      return route.fulfill({ status: 204, headers: cors });
    }
    if (
      route.request().url().includes("/.well-known/oauth-protected-resource")
    ) {
      return route.fulfill({
        status: 200,
        headers: { ...cors, "content-type": "application/json" },
        body: JSON.stringify({
          authorization_servers: [mcp.mcpOrigin],
          resource: `${mcp.mcpOrigin}/mcp`,
        }),
      });
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
  // The URL box only appears once the master toggle is on.
  await page.getByRole("switch", { name: "Trusted MCP server" }).check();
  await page.getByLabel("MCP server URL").fill(`${mcp.mcpOrigin}/mcp`);
  await page.getByRole("button", { name: "Trust this server" }).click();
  // Once a server is trusted the input is replaced by the server row, which
  // carries a remove button — a unique assertion that it was added.
  await expect(
    page.getByRole("button", { name: "Remove this server" }),
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
  test.slow();
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

test("Allow access registers the server's session key", async ({
  page,
  mcp,
}) => {
  test.slow();
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  // The read-only feature is flagged off, so the access-level toggle is
  // hidden and the connection is full access (a queries-only delegation would
  // fail closed in every current agent — see the READ_ONLY_MODE flag).
  await expect(
    page.getByRole("checkbox", { name: "Read-only mode" }),
  ).toHaveCount(0);
  await page.getByRole("button", { name: "Allow access" }).click();

  // The connect flow fetched the server's session key, registered it with the
  // backend, and reported completion: the state echo, the grant expiration
  // (ns since epoch as a decimal string), and the access level the user chose.
  // No delegation chain travels anywhere.
  const completion = await mcp.completion;
  expect(completion.state).toBe(mcp.state);
  expect(completion.expiration).toMatch(/^\d+$/);
  expect(expirationMillis(completion.expiration)).toBeGreaterThan(Date.now());
  // With READ_ONLY_MODE flagged off the session is full access, and the server
  // is told so up front via the completion's `permissions` field ("all") — no
  // probe delegation needed. (The read-only value "queries" is exercised by the
  // utils unit tests, which drive mcpAuthorize directly with the flag's effect.)
  expect(completion.permissions).toBe("all");
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});

test("A finish_url from the server hands the tab back to it after connecting", async ({
  page,
  mcp,
}) => {
  test.slow();
  await addVirtualAuthenticator(page);
  // The server asks for the browser back after the connect (its key response
  // carries `finish_url`) — how a real server completes its own flow, e.g.
  // minting the OAuth code for an MCP client and redirecting back to it.
  mcp.enableFinishRedirect();
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await page.getByRole("button", { name: "Allow access" }).click();

  // The session registers and completion is reported like any connect...
  const completion = await mcp.completion;
  expect(completion.state).toBe(mcp.state);
  // ...but instead of II's close screen, the tab lands on the server's
  // finish URL.
  await page.waitForURL(mcp.finishUrl);
  await expect(
    page.getByRole("heading", { name: "Connection complete" }),
  ).toBeVisible();
});

test("The finish_secret never leaks to logs, errors, or other requests", async ({
  page,
  mcp,
}) => {
  test.slow();
  // The server's key response carries a finish_url whose query holds a one-time
  // finish_secret (the H3 "Consent-Bound Completion" credential the server hands
  // only to the consenting browser). II must navigate the tab to that URL and
  // surface the secret nowhere else: not to the console, not through an uncaught
  // error, and not on any outbound request but the finish navigation itself.
  // Anything else would leak the credential to a log/telemetry sink an attacker
  // (or a bug report) could read — which is exactly what P2 forbids. This covers
  // the +page.svelte funnel/error surface the utils unit test can't reach.
  await addVirtualAuthenticator(page);
  mcp.enableFinishRedirect();
  const secret = mcp.finishSecret;

  // Attach the observers before anything loads, so nothing is missed. The
  // finish navigation (a GET to /oauth/finish) is the one place the secret is
  // meant to appear, so it's excluded; every other request — URL and body — is
  // inspected.
  const consoleLeaks: string[] = [];
  page.on("console", (msg) => {
    if (msg.text().includes(secret)) consoleLeaks.push(msg.text());
  });
  const errorLeaks: string[] = [];
  page.on("pageerror", (error) => {
    if (error.message.includes(secret)) errorLeaks.push(error.message);
  });
  const requestLeaks: string[] = [];
  page.on("request", (request) => {
    const url = request.url();
    if (url.includes("/oauth/finish")) {
      return;
    }
    if (url.includes(secret) || (request.postData() ?? "").includes(secret)) {
      requestLeaks.push(`${request.method()} ${url}`);
    }
  });

  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  await page.goto(mcp.buildAuthorizeUrl({ app: APP }));
  await page.getByRole("button", { name: "Allow access" }).click();

  // Drive the full connect through to the finish navigation — the secret has
  // now flowed through the key response, +page.svelte, and the redirect.
  await mcp.completion;
  await page.waitForURL(mcp.finishUrl);
  await expect(
    page.getByRole("heading", { name: "Connection complete" }),
  ).toBeVisible();

  expect(consoleLeaks).toEqual([]);
  expect(errorLeaks).toEqual([]);
  expect(requestLeaks).toEqual([]);
});

test("Identity switcher shows while signing in and hides on the success screen", async ({
  page,
  mcp,
}) => {
  test.slow();
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
  test.slow();
  // A non-preset value (2 hours), in seconds, to exercise that an arbitrary
  // requested TTL is honoured exactly — not snapped to a dropdown preset.
  const ttlSeconds = 2 * 60 * 60;
  await addVirtualAuthenticator(page);
  await mcp.installInterceptor(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await mcp.trustServer(page);

  const before = Date.now();
  await page.goto(mcp.buildAuthorizeUrl({ app: APP, ttlSeconds }));
  await page.getByRole("button", { name: "Allow access" }).click();

  const expMillis = expirationMillis((await mcp.completion).expiration);
  const requestedMillis = ttlSeconds * 1000;
  expect(expMillis - before).toBeGreaterThanOrEqual(requestedMillis - 60_000);
  expect(expMillis - before).toBeLessThanOrEqual(requestedMillis + 60_000);
});

// The browser /mcp flow registers the MCP server's session key for the user's
// identity (fetched from the callback the request identifies — not a
// per-request app, and no account is chosen at connect); per-app delegations
// are minted server-side by the `mcp_prepare/get_delegation` canister methods,
// with the app account chosen there. Grant semantics (expiry, revocation,
// replacement) are canister logic, covered by the integration tests
// (tests/integration/mcp.rs), not here.
