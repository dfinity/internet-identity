import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { TEST_APP_CANONICAL_URL } from "../../../utils";
import { continueAs, SESSION_SIGN_IN, signInAsFirstIdentity } from "./helpers";

/**
 * What a sign-in leaves behind: an account the app acts as, a delegation to act
 * with, and one sign-in per browser rather than one per attempt.
 *
 * Runs the "Signing in" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — FIRST-1 and FIRST-3 — plus the
 * per-app account, which the designs promise and no scenario there names.
 */
test.describe("signing in", () => {
  test.use({ authorizeConfig: SESSION_SIGN_IN });

  test.describe("a first sign-in leaves the app holding a session and a delegation", () => {
    test.afterEach(async ({ signedInApp }) => {
      await expect(signedInApp.account).not.toHaveText("-");
      // The ceremony mints before it resolves, so a delegation is held straight
      // away rather than only once the tab next comes forward.
      await expect(signedInApp.delegation).not.toHaveText("none held");
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test("signing in twice from one browser replaces the session rather than adding one", async ({
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await testApp.open();
    await testApp.signIn(authenticate);
    const before = await testApp.sessionKey.textContent();

    await testApp.signIn(authenticate);

    await expect(testApp.sessionKey).not.toHaveText(before ?? "");
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
    const here = await testApp.account.textContent();

    // The same app on another origin. `NOT_TEST_APP_URL` is not the app at all:
    // every unknown host resolves to the II dev server.
    const elsewhere = openTestApp(await context.newPage());
    await elsewhere.open({ url: TEST_APP_CANONICAL_URL });
    await elsewhere.signIn(authenticate);

    await expect(elsewhere.account).not.toHaveText(here ?? "");
    await elsewhere.close();
  });
});
