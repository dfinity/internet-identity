import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { TEST_APP_CANONICAL_URL } from "../../../utils";
import {
  continueAs,
  listedBrowsers,
  openSettings,
  signInAsFirstIdentity,
} from "./helpers";

/**
 * What a sign-in leaves behind: an account the app acts as, a delegation to act
 * with, and one sign-in per browser rather than one per attempt.
 *
 * Runs the "Signing in" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — FIRST-1 and FIRST-3 — plus the
 * per-app account, which the designs promise and no scenario there names.
 */
test.describe("signing in", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test.describe("a first sign-in leaves the app holding a session and a delegation", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.expectHoldsAccount();
      // The ceremony mints before it resolves, so a delegation is held straight
      // away rather than only once the tab next comes forward.
      await signedInApp.expectHoldsDelegation();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test("signing in twice from one browser leaves one sign-in, not two", async ({
    testApp,
    browser,
    identities,
    signInWithIdentity,
  }) => {
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    // FIRST-3 is about the browser entry rather than the session key: a key that
    // rotated proves only that, and a second ceremony that had registered a
    // second browser would rotate one too. The count is what says the browser
    // was recognised as itself.
    //
    // Read from another browser, because a browser reading its own list sees
    // itself as the one it cannot sign out — the row for the browser being
    // looked at carries no button, by design.
    const onlooker = await browser.newContext({ ignoreHTTPSErrors: true });
    try {
      const settings = await openSettings(
        onlooker,
        identities[0].identityNumber,
        signInWithIdentity,
      );
      await expect(listedBrowsers(settings)).toHaveCount(1);
      await settings.close();
    } finally {
      await onlooker.close();
    }
  });

  test("one identity is a different account at each app", async ({
    testApp,
    openTestApp,
    context,
    identities,
    signInWithIdentity,
  }) => {
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    const here = await testApp.accountPrincipal();

    // The same app on another origin. `NOT_TEST_APP_URL` is not the app at all:
    // every unknown host resolves to the II dev server.
    const elsewhere = openTestApp(await context.newPage());
    await elsewhere.open({ url: TEST_APP_CANONICAL_URL });
    await elsewhere.signIn(authenticate);

    await elsewhere.expectAccountOtherThan(here);
    await elsewhere.close();
  });
});
