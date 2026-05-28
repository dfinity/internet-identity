import { expect, type Page } from "@playwright/test";
import { test } from "../fixtures";
import { addVirtualAuthenticator, II_URL } from "../utils";

/** Builds the {@link https://id.ai/cli} URL with the CLI params in the
 *  fragment (matching the CLI wire format). */
const cliUrl = (params: {
  publicKey: string;
  callbackUrl: string;
  appHost?: string;
  ttlMinutes?: number;
}): string => {
  const fragment = new URLSearchParams();
  fragment.set("public_key", params.publicKey);
  fragment.set("callback", params.callbackUrl);
  if (params.appHost !== undefined) {
    fragment.set("app", params.appHost);
  }
  if (params.ttlMinutes !== undefined) {
    fragment.set("ttl", String(params.ttlMinutes));
  }
  return `${II_URL}/cli#${fragment.toString()}`;
};

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

/** Signs in with the existing discoverable passkey on the current page. */
const signInExisting = async (page: Page): Promise<void> => {
  await page.getByRole("button", { name: "Continue with passkey" }).click();
  await page.getByRole("button", { name: "Use existing identity" }).click();
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

test("Non-loopback callback is rejected", async ({ page, cli }) => {
  await page.goto(
    cliUrl({
      publicKey: cli.publicKey,
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
  await page.goto(
    cliUrl({ publicKey: cli.publicKey, callbackUrl: cli.callbackUrl }),
  );
  await signUp(page);
  const delegation = cli.captureDelegation(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const body = await delegation;
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

test("App mode without CLI access enabled shows the gated error screen", async ({
  page,
  cli,
}) => {
  await addVirtualAuthenticator(page);
  await page.goto(
    cliUrl({
      publicKey: cli.publicKey,
      callbackUrl: cli.callbackUrl,
      appHost: "nice-name.com",
    }),
  );
  await signUp(page);
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
});

test("Requested TTL within bounds is honoured", async ({ page, cli }) => {
  const ttlMinutes = 60; // 1 hour
  await addVirtualAuthenticator(page);
  const before = Date.now();
  await page.goto(
    cliUrl({
      publicKey: cli.publicKey,
      callbackUrl: cli.callbackUrl,
      ttlMinutes,
    }),
  );
  await signUp(page);
  const delegation = cli.captureDelegation(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const expMillis = expirationMillis(await delegation);
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
  const before = Date.now();
  await page.goto(
    cliUrl({
      publicKey: cli.publicKey,
      callbackUrl: cli.callbackUrl,
      ttlMinutes,
    }),
  );
  await signUp(page);
  const delegation = cli.captureDelegation(page);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const expMillis = expirationMillis(await delegation);
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

  await page.getByRole("link", { name: "Settings" }).click();
  await page.waitForURL(II_URL + "/manage/settings");
  await page.getByRole("switch").click();
  await page
    .getByLabel("I'm using the official ICP CLI and I trust this device.")
    .check();
  await page.getByRole("button", { name: "Enable CLI access" }).click();
  await expect(page.getByText("Enabled", { exact: true })).toBeVisible();

  // Re-authenticate on /cli with the same discoverable passkey; the
  // device-local CLI access flag persists in localStorage for this identity.
  await page.goto(
    cliUrl({
      publicKey: cli.publicKey,
      callbackUrl: cli.callbackUrl,
      appHost: "nice-name.com",
    }),
  );
  await signInExisting(page);
  const delegation = cli.captureDelegation(page);
  await page.getByRole("button", { name: "Allow access" }).click();

  expect(await delegation).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});
