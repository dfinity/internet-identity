import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { continueAs, SESSION_SIGN_IN, signInAsFirstIdentity } from "./helpers";

/**
 * What a browser keeps between visits, and what it must not. Clearing the site's
 * data is a clean start, an abandoned sign-in can be tried again, and two
 * identities used from the same browser never see each other's account.
 *
 * Runs the "Coming back later" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — STAY-2, STAY-3 and STAY-4. STAY-1
 * needs the browser closed and reopened, which a test cannot do to itself.
 */
test.describe("coming back later", () => {
  test.use({ authorizeConfig: SESSION_SIGN_IN });

  test.describe("clearing the site's data is a clean start", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.clearSiteData();
      await signedInApp.reload();

      await expect(signedInApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test("an interrupted sign-in can be retried", async ({
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await testApp.open();
    await testApp.abandonSignIn(async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
    });
    await expect(testApp.state).toHaveText("no session");

    // DEV-13: the browser persisted its key before the call, so a second attempt
    // is recognised as the same browser rather than blocked by the first.
    await testApp.signIn(
      continueAs(identities[0].identityNumber, signInWithIdentity),
    );
    await expect(testApp.state).toHaveText("signed in");
  });

  test.describe("with two identities", () => {
    test.use({
      identityConfig: {
        createIdentities: [{ name: "First user" }, { name: "Second user" }],
      },
    });

    test("two identities in one browser stay apart", async ({
      testApp,
      identities,
      signInWithIdentity,
    }) => {
      await testApp.open();
      await testApp.signIn(
        continueAs(identities[0].identityNumber, signInWithIdentity),
      );
      await expect(testApp.account).not.toHaveText("-");
      const first = await testApp.account.textContent();

      await testApp.signOut();
      await expect(testApp.state).toHaveText("no session");

      await testApp.signIn(
        continueAs(identities[1].identityNumber, signInWithIdentity),
      );

      await expect(testApp.account).not.toHaveText(first ?? "");
    });
  });
});
