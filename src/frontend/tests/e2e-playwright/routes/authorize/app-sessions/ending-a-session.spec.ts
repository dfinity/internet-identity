import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { TEST_APP_CANONICAL_URL } from "../../../utils";
import {
  continueAs,
  forgetThisBrowser,
  listedBrowsers,
  openSettings,
  signInAsFirstIdentity,
  signOutFirstBrowser,
} from "./helpers";

/**
 * Access that can be ended is the point of the design, so these are the scenarios
 * about ending it: by the app signing out, and by the identity's owner signing a
 * whole browser out from settings. Ending it reaches an app that is running,
 * leaves nothing to come back from, and touches nothing else.
 *
 * Runs the "Ending a session" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — EXIT-1, EXIT-3, EXIT-5 and EXIT-6 —
 * and the silent re-issue that has nothing left to answer from.
 */
test.describe("ending a session", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test.describe("signing out leaves nothing behind, across a reload", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.signOut();
      await signedInApp.expectSignedOut();

      await signedInApp.reload();
      await signedInApp.expectSignedOut();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("a silent re-issue with nothing to answer from fails one way", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.signOut();
      await signedInApp.expectSignedOut();

      await signedInApp.silentReauth();

      // FAIL-1 and SIL-2: one outcome, reported without asking the user
      // anything, and nothing created. Windows are not counted: the window
      // transport opens a channel either way, and SIL-1 is about screens, which
      // the redirect transport the silent design targets is what makes
      // checkable.
      await signedInApp.expectSilentReauthFailed();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test("signing this browser out ends every app it signed into", async ({
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
    // EXIT-3 says several apps, because one app only proves one session was
    // ended: what the browser-wide action claims is that it reaches all of them.
    await testApp.open();
    await testApp.signIn(authenticate);

    const other = openTestApp(await context.newPage());
    await other.open({ url: TEST_APP_CANONICAL_URL });
    await other.signIn(authenticate);

    // This browser is the one that signed in, and a browser cannot sign itself
    // out from the list — that row carries no button. Its route is the identity's
    // own sign-out, where forgetting revokes rather than merely leaving.
    const settings = await openSettings(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await forgetThisBrowser(settings);
    await settings.close();

    // END-5 allows an app to keep working until the delegation it holds expires,
    // so nothing shows until one is due. It is the mint that then discovers the
    // session is gone.
    for (const app of [testApp, other]) {
      await app.focus();
      await app.ageDelegation();
      await app.replaceDelegation();
      await app.expectSignedOut();
    }
    await other.close();
  });

  test("signing another browser out from the list ends its access alone", async ({
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

    // The other half of EXIT-3: the owner looking at their list from somewhere
    // else, where the browser that signed in is another browser and does carry a
    // button.
    const onlooker = await browser.newContext({ ignoreHTTPSErrors: true });
    try {
      const settings = await openSettings(
        onlooker,
        identities[0].identityNumber,
        signInWithIdentity,
      );
      await expect(listedBrowsers(settings)).toHaveCount(1);
      await signOutFirstBrowser(settings);
      await settings.close();
    } finally {
      await onlooker.close();
    }

    await testApp.focus();
    await testApp.ageDelegation();
    await testApp.replaceDelegation();
    await testApp.expectSignedOut();
  });

  // The two below sign in twice, which `authorizePage` does not do, so they drive
  // the app themselves.

  test("signing out of one app leaves the other alone", async ({
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

    const other = openTestApp(await context.newPage());
    await other.open({ url: TEST_APP_CANONICAL_URL });
    await other.signIn(authenticate);

    await testApp.focus();
    await testApp.signOut();
    await testApp.expectSignedOut();

    // Another origin is another account and another session.
    await other.waitUntilSignedIn();
    await other.close();
  });

  test("a browser signed out is still the same browser", async ({
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

    // Read and signed out from elsewhere, because the list offers no button for
    // the browser reading it: signing out the browser in front of you goes
    // through the identity's own sign-out instead.
    const onlooker = await browser.newContext({ ignoreHTTPSErrors: true });
    try {
      const settings = await openSettings(
        onlooker,
        identities[0].identityNumber,
        signInWithIdentity,
      );
      await expect(listedBrowsers(settings)).toHaveCount(1);
      await signOutFirstBrowser(settings);
      await settings.close();

      // DEV-18: the entry stays, so signing in again reuses it rather than
      // adding a second.
      await testApp.focus();
      await testApp.signIn(authenticate);
      await testApp.waitUntilSignedIn();

      const again = await openSettings(
        onlooker,
        identities[0].identityNumber,
        signInWithIdentity,
      );
      await expect(listedBrowsers(again)).toHaveCount(1);
      await again.close();
    } finally {
      await onlooker.close();
    }
  });
});
