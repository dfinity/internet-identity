import { expect, type Page } from "@playwright/test";
import { Principal } from "@icp-sdk/core/principal";
import { test } from "../fixtures";
import { addVirtualAuthenticator, authorize, II_URL } from "../utils";

/** Decodes a hex string (as delegation chains encode public keys) to bytes. */
const hexToBytes = (hex: string): Uint8Array =>
  Uint8Array.from(hex.match(/.{1,2}/g) ?? [], (byte) => parseInt(byte, 16));

const cliFragment = (params: {
  publicKey: string;
  callbackUrl: string;
}): string => {
  const fragment = new URLSearchParams();
  fragment.set("public_key", params.publicKey);
  fragment.set("callback", params.callbackUrl);
  return fragment.toString();
};

const signUp = async (page: Page): Promise<void> => {
  const continueWithPasskey = page.getByRole("button", {
    name: "Continue with passkey",
  });
  const signUpToggle = page.getByRole("button", {
    name: "Sign up",
    exact: true,
  });
  // Wait for whichever entry the surface renders: /cli's mode="both" picker
  // exposes "Continue with passkey", while the homepage's mode="signin"
  // picker exposes the "Sign up" toggle instead. We probe before branching,
  // so the next .isVisible() needs at least one of them committed to the DOM.
  await continueWithPasskey.or(signUpToggle).first().waitFor();
  if (await continueWithPasskey.isVisible()) {
    await continueWithPasskey.click();
    await page.getByRole("button", { name: "Create new identity" }).click();
  } else {
    await signUpToggle.click();
    await page.getByRole("button", { name: "Sign up with passkey" }).click();
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
  // Delegations expose `expiration` as a hex-encoded u64 of nanoseconds.
  return Number(BigInt(`0x${expiration}`) / BigInt(1_000_000));
};

/**
 * Enables device-local CLI access for the signed-in identity via Settings.
 * Assumes the page is already on `/manage`.
 *
 * Branches on the `isMobile` fixture to open the hamburger nav (mobile only):
 * the previous `if (await menuButton.isVisible())` guard was a non-waiting
 * check that raced page hydration and intermittently left the menu closed, so
 * the subsequent "Settings" click timed out on the mobile project.
 */
const enableCliAccessInSettings = async (
  page: Page,
  isMobile: boolean,
): Promise<void> => {
  if (isMobile) {
    await page.getByRole("button", { name: "Open menu" }).click();
  }
  await page.getByRole("link", { name: "Settings" }).click();
  await page.waitForURL(II_URL + "/manage/settings");
  await page.getByRole("switch", { name: "CLI access" }).check();
  await page
    .getByLabel("I'm using the official ICP CLI and I trust this device.")
    .check();
  await page.getByRole("button", { name: "Enable CLI access" }).click();
  await expect(page.getByText("Enabled", { exact: true })).toBeVisible();
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
    await cli.resolveAuthorizeUrl(page, { domain: "nice-name.com" }),
  );
  await signUp(page);
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
});

test("Returning user without CLI access lands on the gated screen immediately", async ({
  page,
  cli,
}) => {
  // Establish a last-used identity (CLI access left disabled), the way a
  // returning user would have.
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  // App-mode /cli for that identity: since CLI access isn't enabled on this
  // device, the gated screen shows up front — no "Allow access" / sign-in step.
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { domain: "nice-name.com" }),
  );
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
  await expect(page.getByRole("button", { name: "Allow access" })).toBeHidden();
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
  isMobile,
}) => {
  // Sign up a fresh identity on the landing page, then enable CLI access for
  // it via the Settings page (SPA navigation keeps the session).
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");

  await enableCliAccessInSettings(page, isMobile);

  // Re-authenticate on /cli; the device-local CLI access flag persists in
  // localStorage for this identity. With a last-used identity present, /cli
  // opens directly on the Allow-access screen (like /authorize) — clicking it
  // authenticates that identity and posts the delegation.
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { domain: "nice-name.com" }),
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

/** The hex root public key of a delegation chain (the per-origin account key). */
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

test("`--app` derives a different identity than generic mode for the same identity", async ({
  page,
  cli,
  isMobile,
}) => {
  // One identity, with device CLI access enabled so app mode isn't gated.
  await addVirtualAuthenticator(page);
  await page.goto(II_URL);
  await signUp(page);
  await page.waitForURL(II_URL + "/manage");
  await enableCliAccessInSettings(page, isMobile);

  // Generic sign-in (no `domain`) → delegation rooted at the auth page's
  // default origin (cli.id.ai).
  await page.goto(await cli.resolveAuthorizeUrl(page));
  await page.getByRole("button", { name: "Continue", exact: true }).click();
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();

  // App-mode sign-in for the same identity with `domain=nice-name.com` →
  // delegation rooted at that origin instead. Navigate off /cli first so the
  // second visit is a full load (a fragment-only change wouldn't re-run `load`).
  await page.goto(II_URL);
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { domain: "nice-name.com" }),
  );
  await page.getByRole("button", { name: "Allow access" }).click();
  await expect(
    page.getByRole("heading", { name: "You're signed in" }),
  ).toBeVisible();

  // Same identity, two derivation origins → two distinct root principals. If
  // `domain` were ignored, these would collide.
  expect(cli.receivedDelegations.length).toBe(2);
  expect(rootPublicKey(cli.receivedDelegations[1])).not.toBe(
    rootPublicKey(cli.receivedDelegations[0]),
  );
});

test("`--app` links the same principal that /authorize gives for that app", async ({
  page,
  cli,
  identities,
  signInWithIdentity,
  isMobile,
}) => {
  const identityNumber = identities[0].identityNumber;

  // Principal the app (nice-name.com) sees via the normal /authorize flow.
  const authorizePrincipal = await authorize(page, async (authPage) => {
    await signInWithIdentity(authPage, identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  });

  // Enable device CLI access for the same identity so app mode isn't gated.
  await page.goto(II_URL);
  await signInWithIdentity(page, identityNumber);
  await page.waitForURL(II_URL + "/manage");
  await enableCliAccessInSettings(page, isMobile);

  // Principal the CLI links via `icp identity link web --app nice-name.com`: the
  // self-authenticating principal of the delegation chain's root public key.
  await page.goto(
    await cli.resolveAuthorizeUrl(page, { domain: "nice-name.com" }),
  );
  await page.getByRole("button", { name: "Allow access" }).click();
  const chain = await cli.receivedDelegation;
  const cliPrincipal = Principal.selfAuthenticating(
    hexToBytes(rootPublicKey(chain)),
  ).toText();

  // Same identity + same derivation origin (nice-name.com) ⇒ same principal.
  expect(cliPrincipal).toBe(authorizePrincipal);
});
