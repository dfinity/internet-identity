import { expect } from "@playwright/test";
import { test } from "../fixtures";
import { II_URL } from "../utils";

const cliUrl = (params: {
  publicKeyHex: string;
  callbackUrl: string;
  appHost?: string;
}): string => {
  const url = new URL(II_URL + "/cli");
  url.searchParams.set("public_key", params.publicKeyHex);
  url.searchParams.set("callback", params.callbackUrl);
  if (params.appHost !== undefined) {
    url.searchParams.set("app", params.appHost);
  }
  return url.toString();
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
  await page.goto(II_URL);
  await signInWithIdentity(page, identities[0].identityNumber);

  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
    }),
  );
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

test("Dapp mode without CLI access enabled shows the gated error screen", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  await page.goto(II_URL);
  await signInWithIdentity(page, identities[0].identityNumber);

  await page.goto(
    cliUrl({
      publicKeyHex: cli.publicKeyHex,
      callbackUrl: cli.callbackUrl,
      appHost: "nice-name.com",
    }),
  );
  await expect(
    page.getByRole("heading", { name: "CLI access not enabled" }),
  ).toBeVisible();
});

test("Dapp mode succeeds once CLI access is enabled in Settings", async ({
  page,
  identities,
  signInWithIdentity,
  cli,
}) => {
  await page.goto(II_URL);
  await signInWithIdentity(page, identities[0].identityNumber);

  // Enable CLI access for this identity in the Settings page.
  await page.goto(II_URL + "/manage/settings");
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
