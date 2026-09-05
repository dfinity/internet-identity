import { expect, type Locator, type Page } from "@playwright/test";
import { II_URL } from "../../../utils";

/**
 * Shared by the session specs in this directory, which are grouped the way
 * `docs/ongoing/session-test-scenarios.md` groups the scenarios they run.
 */

/** The commonest sign-in: pick an identity, then continue. */
export const continueAs =
  (
    identityNumber: bigint,
    signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>,
  ) =>
  async (authPage: Page): Promise<void> => {
    await signInWithIdentity(authPage, identityNumber);
    await authPage
      .getByRole("button", { name: "Continue", exact: true })
      .click();
  };

/**
 * The identity provider's side of a sign-in, for the specs that let
 * `authorizePage` perform the ceremony. Their bodies are all this, and what the
 * scenario is about happens in `afterEach`.
 */
export const signInAsFirstIdentity = async ({
  authorizePage,
  identities,
  signInWithIdentity,
}: {
  authorizePage: { page: Page };
  identities: { identityNumber: bigint }[];
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>;
}): Promise<void> => {
  await continueAs(
    identities[0].identityNumber,
    signInWithIdentity,
  )(authorizePage.page);
};

/** The identity's settings, signed in, with the browser list on screen. */
export const openSettings = async (
  context: { newPage: () => Promise<Page> },
  identityNumber: bigint,
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>,
): Promise<Page> => {
  const settings = await context.newPage();
  await settings.goto(`${II_URL}/manage/settings`);
  await signInWithIdentity(settings, identityNumber);
  await expect(
    settings.getByRole("heading", { name: "Signed-in browsers" }),
  ).toBeVisible();
  return settings;
};

/** One row per browser the identity is signed in from. */
export const listedBrowsers = (settings: Page): Locator =>
  settings.getByRole("button", { name: "Sign out" });

/** Fails unless the identity's settings list at least one browser. */
export const expectBrowserListed = async (settings: Page): Promise<void> => {
  await expect(listedBrowsers(settings).first()).toBeVisible();
};

/**
 * Signs the first listed browser out and waits for settings to say it is done.
 *
 * Ending a sign-in is a canister call, so this waits longer than an assertion
 * about something already on the page would.
 */
export const signOutFirstBrowser = async (settings: Page): Promise<void> => {
  await listedBrowsers(settings).first().click();
  await expect(settings.getByText("Signed out")).toBeVisible({
    timeout: 30_000,
  });
};

/** The domain whose subdomains share a session in the sibling scenarios. */
export const SHARED_DOMAIN = "nice-name.com";
