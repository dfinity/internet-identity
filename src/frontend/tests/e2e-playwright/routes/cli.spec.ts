import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";

const cliFragment = (params: {
  publicKey: string;
  callbackUrl: string;
}): string => {
  const fragment = new URLSearchParams();
  fragment.set("public_key", params.publicKey);
  fragment.set("callback", params.callbackUrl);
  return fragment.toString();
};

/** Signs up a fresh identity from the inline auth wizard on the current page. */
const signUp = async (page: Page): Promise<void> => {
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  await page.getByRole("button", { name: "Create new identity" }).click();
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
  // Delegations expose `expiration` as a hex-encoded u64 of nanoseconds.
  return Number(BigInt(`0x${expiration}`) / BigInt(1_000_000));
};

test("cli.id.ai redirects to id.ai/cli, preserving the fragment", async ({
  page,
  cli,
}) => {
  const fragment = cliFragment({
    publicKey: cli.publicKey,
    callbackUrl: cli.callbackUrl,
  });
  await page.goto(`https://cli.id.ai/#${fragment}`);
  await page.waitForURL(`${II_URL}/cli**`);
  expect(page.url()).toBe(`${II_URL}/cli#${fragment}`);
});

test("Invalid params show the error screen", async ({ page }) => {
  await page.goto(II_URL + "/cli");
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("/.well-known/cli-auth-config advertises the CLI flow path", async ({
  page,
}) => {
  // Same-origin fetch from the II page (the harness can't reach the
  // dev-server-served canister via Playwright's request context).
  await page.goto(II_URL);
  const result = await page.evaluate(async () => {
    const response = await fetch("/.well-known/cli-auth-config");
    return {
      contentType: response.headers.get("content-type"),
      body: await response.text(),
    };
  });
  expect(result.contentType).toContain("application/json");
  expect(JSON.parse(result.body)).toEqual({ path: "/cli" });
});

test("Non-loopback callback is rejected", async ({ page, cli }) => {
  await page.goto(
    await cli.resolveAuthorizeUrl(page, {
      callbackUrl: "https://attacker.example.com/cb",
    }),
  );
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("Generic CLI sign-in posts a two-hop delegation chain to the loopback callback", async ({
  page,
  cli,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(await cli.resolveAuthorizeUrl(page));
  await signUp(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const body = await cli.receivedDelegation;
  expect(body).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  // Rooted at the user's principal: canister-signed delegation to the
  // ephemeral browser key, then the ephemeral key's sub-delegation to the
  // CLI's public key.
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
  cli,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(await cli.resolveAuthorizeUrl(page));
  await signUp(page);

  // On the authorize step the user is still choosing how to sign in, so the
  // header switcher is available.
  const switcher = page.getByRole("button", { name: "Switch identity" });
  await expect(
    page.getByRole("button", { name: "Continue", exact: true }),
  ).toBeVisible();
  await expect(switcher).toBeVisible();

  // Once authorized and back on the success screen there's nothing to sign in
  // to, so the switcher is gone.
  await page.getByRole("button", { name: "Continue", exact: true }).click();
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
  await expect(switcher).toBeHidden();
});

test("Returning user opens on the Continue screen, not the method wizard", async ({
  page,
  cli,
}) => {
  // Establish a last-used identity (persisted in localStorage), the way a user
  // who has signed in before would have.
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  // Like /authorize, /cli should now open directly on the Continue screen for
  // that identity rather than the sign-in method wizard.
  await page.goto(await cli.resolveAuthorizeUrl(page));
  await expect(
    page.getByRole("button", { name: "Continue", exact: true }),
  ).toBeVisible();
  await expect(
    page.getByRole("button", { name: "Continue with passkey" }),
  ).toBeHidden();

  // Continue authenticates the last-used identity and posts the delegation.
  await page.getByRole("button", { name: "Continue", exact: true }).click();
  expect(await cli.receivedDelegation).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});

test("Identity mismatch shows a toast and lets the user retry in place", async ({
  page,
  cli,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(await cli.resolveAuthorizeUrl(page));
  await signUp(page);

  // The CLI rejects the first delegation as a principal mismatch (the
  // `login` re-auth path) and redirects back with status=identity-mismatch.
  cli.setNextOutcome("identity-mismatch");
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  // The redirect-back stays on the Continue screen for the last-used identity
  // (not the method wizard) with a toast explaining why, and the header
  // switcher is available so the user can pick a different identity in place.
  await expect(page.getByText("That identity doesn't match")).toBeVisible();
  await expect(
    page.getByRole("button", { name: "Switch identity" }),
  ).toBeVisible();

  // Retry in place: the second attempt succeeds and the delegation reaches the
  // loopback server.
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  expect(await cli.receivedDelegation).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});

test("App mode without CLI access enabled shows the gated error screen", async ({
  page,
  cli,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { appHost: "nice-name.com" }),
  );
  await signUp(page);
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
});

test("Requested TTL within bounds is honoured", async ({ page, cli }) => {
  const ttlMinutes = 60; // 1 hour
  await addVirtualAuthenticator(page);
  const url = await cli.resolveAuthorizeUrl(page, { ttlMinutes });
  const before = Date.now();
  await page.goto(url);
  await signUp(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const expMillis = expirationMillis(await cli.receivedDelegation);
  const requestedMillis = ttlMinutes * 60 * 1000;
  expect(expMillis - before).toBeGreaterThanOrEqual(requestedMillis - 60_000);
  expect(expMillis - before).toBeLessThanOrEqual(requestedMillis + 60_000);
});

test("Requested TTL beyond the backend max is clamped to 30 days", async ({
  page,
  cli,
}) => {
  const ttlMinutes = 60 * 24 * 60; // 60 days — over the canister's 30-day cap.
  await addVirtualAuthenticator(page);
  const url = await cli.resolveAuthorizeUrl(page, { ttlMinutes });
  const before = Date.now();
  await page.goto(url);
  await signUp(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const expMillis = expirationMillis(await cli.receivedDelegation);
  const thirtyDaysMillis = 30 * 24 * 60 * 60 * 1000;
  expect(expMillis - before).toBeLessThanOrEqual(thirtyDaysMillis + 60_000);
  expect(expMillis - before).toBeGreaterThanOrEqual(thirtyDaysMillis - 60_000);
});

test("App mode succeeds once CLI access is enabled in Settings", async ({
  page,
  cli,
}) => {
  // Sign up a fresh identity on the landing page, then enable CLI access for
  // it via the Settings page (SPA navigation keeps the session).
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  // On mobile the sidebar nav is behind a menu button; open it first.
  const menuButton = page.getByRole("button", { name: "Open menu" });
  if (await menuButton.isVisible()) {
    await menuButton.click();
  }
  await page.getByRole("link", { name: "Settings" }).click();
  await page.waitForURL(II_URL + "/manage/settings");
  await page.getByRole("switch").click();
  await page
    .getByLabel("I'm using the official ICP CLI and I trust this device.")
    .check();
  await page.getByRole("button", { name: "Enable CLI access" }).click();
  await expect(page.getByText("Enabled", { exact: true })).toBeVisible();

  // Re-authenticate on /cli; the device-local CLI access flag persists in
  // localStorage for this identity. With a last-used identity present, /cli
  // opens directly on the Allow-access screen (like /authorize) — clicking it
  // authenticates that identity and posts the delegation.
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { appHost: "nice-name.com" }),
  );
  await page.getByRole("button", { name: "Allow access" }).click();

  expect(await cli.receivedDelegation).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});
