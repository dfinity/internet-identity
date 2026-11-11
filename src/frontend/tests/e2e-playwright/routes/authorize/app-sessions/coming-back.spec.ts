import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { continueAs, listedBrowsers, openSettings } from "./helpers";

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
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test("clearing the site's data is a clean start, and signing in works after it", async ({
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
    await testApp.waitUntilSignedIn();

    await testApp.clearSiteData();

    // Opened rather than reloaded, because a clean start makes this a first-time
    // visitor: which identity provider to use was typed into the form, and the
    // app's own copy of that went with the data. A reload would send the retry
    // to the default provider, which is mainnet.
    await testApp.open();
    await testApp.expectSignedOut();

    // The other half of STAY-2: a clean start is only clean if it can be built
    // on. Signing out and never being able to sign in again would pass the half
    // above.
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();
  });

  test("an interrupted sign-in can be retried", async ({
    testApp,
    browser,
    identities,
    signInWithIdentity,
  }) => {
    await testApp.open();
    await testApp.abandonSignIn(async (authPage) => {
      await signInWithIdentity(authPage, identities[0].identityNumber);
    });
    await testApp.expectSignedOut();

    // DEV-13: the browser persisted its key before the call, so a second attempt
    // is recognised as the same browser rather than blocked by the first.
    await testApp.signIn(
      continueAs(identities[0].identityNumber, signInWithIdentity),
    );
    await testApp.waitUntilSignedIn();

    // STAY-3 asks for one entry, not merely a retry that worked: an abandoned
    // attempt that had registered a browser of its own would let the retry
    // succeed and leave two.
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
      const first = await testApp.accountPrincipal();

      await testApp.signOut();
      await testApp.expectSignedOut();

      await testApp.signIn(
        continueAs(identities[1].identityNumber, signInWithIdentity),
      );

      await testApp.expectAccountOtherThan(first);
    });
  });
});
