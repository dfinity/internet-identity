import { expect } from "@playwright/test";
import { test } from "../fixtures";
import { II_URL } from "../utils";

const cliUrl = (params: {
  publicKeyHex: string;
  callbackUrl: string;
  appHost?: string;
  ttlMinutes?: number;
}): string => {
  const url = new URL(II_URL + "/cli");
  url.searchParams.set("public_key", params.publicKeyHex);
  url.searchParams.set("callback", params.callbackUrl);
  if (params.appHost !== undefined) {
    url.searchParams.set("app", params.appHost);
  }
  if (params.ttlMinutes !== undefined) {
    url.searchParams.set("ttl", String(params.ttlMinutes));
  }
  return url.toString();
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
  const first = body.delegations[0];
  const expiration: unknown = first?.delegation?.expiration;
  if (typeof expiration !== "string") {
    throw new Error("delegation expiration missing");
  }
  // Delegations expose `expiration` as a hex-encoded u64 of nanoseconds.
  return Number(BigInt(`0x${expiration}`) / BigInt(1_000_000));
};

test("Invalid params show the error screen", async ({ page }) => {
  await page.goto(II_URL + "/cli");
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("Non-loopback callback is rejected", async ({ page, cli }) => {
  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: "https://attacker.example.com/cb",
    }),
  );
  await expect(
    page.getByRole("heading", { name: "Invalid request" }),
  ).toBeVisible();
});

test("Generic CLI sign-in posts a delegation chain to the loopback callback", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
    }),
  );
  await signInWithIdentity(page, identities[0].identityNumber);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const body = await cli.receivedDelegation;
  expect(body).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});

test("App mode without CLI access enabled shows the gated error screen", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
      appHost: "nice-name.com",
    }),
  );
  await signInWithIdentity(page, identities[0].identityNumber);
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
});

test("Requested TTL within bounds is honoured", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  const ttlMinutes = 60; // 1 hour
  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
      ttlMinutes,
    }),
  );
  const before = Date.now();
  await signInWithIdentity(page, identities[0].identityNumber);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const body = await cli.receivedDelegation;
  const expMillis = expirationMillis(body);
  const requestedMillis = ttlMinutes * 60 * 1000;
  // Honoured within a generous window for canister + network latency.
  expect(expMillis - before).toBeGreaterThanOrEqual(requestedMillis - 60_000);
  expect(expMillis - before).toBeLessThanOrEqual(requestedMillis + 60_000);
});

test("Requested TTL beyond the backend max is clamped to 30 days", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  // 60 days — well over the canister's 30-day cap.
  const ttlMinutes = 60 * 24 * 60;
  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
      ttlMinutes,
    }),
  );
  const before = Date.now();
  await signInWithIdentity(page, identities[0].identityNumber);
  await page.getByRole("button", { name: "Continue", exact: true }).click();

  const body = await cli.receivedDelegation;
  const expMillis = expirationMillis(body);
  const thirtyDaysMillis = 30 * 24 * 60 * 60 * 1000;
  expect(expMillis - before).toBeLessThanOrEqual(thirtyDaysMillis + 60_000);
  expect(expMillis - before).toBeGreaterThanOrEqual(thirtyDaysMillis - 60_000);
});

test("App mode succeeds once CLI access is enabled in Settings", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  // Sign in on the manage page and enable CLI access for this identity.
  await page.goto(II_URL + "/manage/settings");
  await signInWithIdentity(page, identities[0].identityNumber);
  await page.getByRole("switch").click();
  await page
    .getByLabel("I'm using the official ICP CLI and I trust this device.")
    .check();
  await page.getByRole("button", { name: "Enable CLI access" }).click();
  await expect(page.getByText("Enabled")).toBeVisible();

  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
      appHost: "nice-name.com",
    }),
  );
  await signInWithIdentity(page, identities[0].identityNumber);
  await page.getByRole("button", { name: "Allow access" }).click();

  const body = await cli.receivedDelegation;
  expect(body).toMatchObject({
    delegations: expect.any(Array),
    publicKey: expect.any(String),
  });
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();
});
