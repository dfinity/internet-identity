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

/** The identity's devices page, signed in, with the browser list on screen. */
export const openSettings = async (
  context: { newPage: () => Promise<Page> },
  identityNumber: bigint,
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>,
): Promise<Page> => {
  const settings = await context.newPage();
  await settings.goto(`${II_URL}/manage/devices`);
  await signInWithIdentity(settings, identityNumber);
  // The page's own heading, not a section's: browsers are grouped by the machine they
  // run on, so which group headings appear depends on what the identity has signed in
  // from.
  await expect(
    settings.getByRole("heading", { name: "Devices", exact: true }),
  ).toBeVisible();
  return settings;
};

/**
 * The browsers this list offers to sign out, which is every one but the browser
 * doing the looking: a browser reading its own list finds itself under no
 * button, because signing itself out from here is not what that row is for.
 */
export const listedBrowsers = (settings: Page): Locator =>
  settings.getByRole("button", { name: "Sign out", exact: true });

/**
 * Every browser the list shows, the one doing the looking included.
 *
 * Counted by a label each row carries rather than by its button, so a count says
 * how many browsers the identity has rather than how many this browser may end.
 * Not `getByRole('listitem')`: the navigation beside the list is a list too.
 */
export const listedBrowserRows = (settings: Page): Locator =>
  settings.getByText("First seen", { exact: true });

/**
 * Ends this browser's own sessions, which is what signing out and choosing to be
 * forgotten does.
 *
 * The list's own buttons cannot: they end another browser's sessions, and the
 * browser reading the list is the one case that is not another. Its route is the
 * sign-out the identity offers, where forgetting is what revokes rather than
 * merely leaving.
 */
export const forgetThisBrowser = async (settings: Page): Promise<void> => {
  await settings.getByRole("button", { name: "Switch identity" }).click();
  await settings
    .getByRole("group")
    .getByRole("button", { name: "Sign Out" })
    .click();
  await expect(
    settings.getByRole("heading", { name: "Remember this browser?" }),
  ).toBeVisible();
  // Exact, so it does not also match the "Forgetting..." the button becomes
  // while the sessions are being revoked.
  await settings.getByRole("button", { name: "Forget", exact: true }).click();
  // Forgetting revokes before it navigates, so this waits on a canister call.
  await settings.waitForURL(II_URL, { timeout: 30_000 });
};

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
  await confirmSignOut(settings);
  // Exact: the toast that confirms it says "Signed out of all apps", so the
  // substring matches the row's label and the toast both.
  await expect(settings.getByText("Signed out", { exact: true })).toBeVisible({
    timeout: 30_000,
  });
};

/** Answers the dialog that a row's sign-out opens. */
export const confirmSignOut = async (settings: Page): Promise<void> => {
  await settings
    .getByRole("button", { name: "Sign out of all apps", exact: true })
    .click();
};

/** The domain whose subdomains share a session in the sibling scenarios. */
export const SHARED_DOMAIN = "nice-name.com";
