import { expect, type Page } from "@playwright/test";
import { test } from "../../fixtures";
import { II_URL } from "../../utils";
import { continueAs } from "../authorize/app-sessions/helpers";

/**
 * The page an app's installed web app on an iPhone opens in Safari to link the two, so
 * the app's notifications arrive once. It claims the one-shot key the installed app
 * registered, for whichever identity this Safari is signed in to.
 *
 * Only the links that find nothing to claim are here: registering the key takes the installed app's own session,
 * which the test app does not hold.
 */

const LINK_PATH = "/ios-installed-app";

/** A one-shot key as the installed app puts it in the fragment: `d`, `x` and `y` of a
 *  fresh P-256 key, base64url. Nothing registered it, so no identity awaits it. */
const unregisteredLinkKey = async (): Promise<string> => {
  const keyPair = await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["sign", "verify"],
  );
  const { d, x, y } = await crypto.subtle.exportKey("jwk", keyPair.privateKey);
  const bytes = [d, x, y].map((coordinate) => {
    if (coordinate === undefined) {
      throw new Error("a P-256 JWK names every coordinate");
    }
    return Buffer.from(coordinate, "base64url");
  });
  return Buffer.concat(bytes).toString("base64url");
};

const openTheLink = async (page: Page): Promise<void> => {
  const link = new URL(LINK_PATH, II_URL);
  link.hash = `key=${await unregisteredLinkKey()}`;
  await page.goto(link.toString());
};

const expectNothingToChange = async (page: Page): Promise<void> => {
  await expect(
    page.getByRole("heading", { name: "Nothing to change" }),
  ).toBeVisible({ timeout: 30_000 });
};

test.describe("Linking an installed app in Safari", () => {
  test("finds nothing to change for a link nobody registered for the identity signed in here", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await testApp.open({ protocol: "session" });
    await testApp.signIn(
      continueAs(identities[0].identityNumber, signInWithIdentity),
    );
    await testApp.waitUntilSignedIn();

    const linkPage = await context.newPage();
    await openTheLink(linkPage);

    await expectNothingToChange(linkPage);
    // The key is gone from the URL as soon as the page has read it.
    expect(new URL(linkPage.url()).hash).toBe("");
  });

  test("finds nothing to change in a Safari signed in to no identity", async ({
    browser,
  }) => {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      await openTheLink(page);

      await expectNothingToChange(page);
    } finally {
      await context.close();
    }
  });
});
