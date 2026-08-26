import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import { TEST_APP_CANONICAL_URL } from "../../../utils";
import {
  continueAs,
  openSettings,
  SESSION_SIGN_IN,
  signInAsFirstIdentity,
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
  test.use({ authorizeConfig: SESSION_SIGN_IN });

  test.describe("signing out leaves nothing behind, across a reload", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.signOut();
      await expect(signedInApp.state).toHaveText("no session");

      await signedInApp.reload();
      await expect(signedInApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("a silent re-issue with nothing to answer from fails one way", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.signOut();
      await expect(signedInApp.state).toHaveText("no session");

      await signedInApp.silentReauth();

      // FAIL-1 and SIL-2: one outcome, reported without asking the user
      // anything, and nothing created. Windows are not counted: the window
      // transport opens a channel either way, and SIL-1 is about screens, which
      // the redirect transport the silent design targets is what makes
      // checkable.
      await expect(signedInApp.log).toContainText("error", { timeout: 30_000 });
      await expect(signedInApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("signing the browser out from settings ends the app's access", () => {
    test.afterEach(
      async ({ signedInApp, context, identities, signInWithIdentity }) => {
        const settings = await openSettings(
          context,
          identities[0].identityNumber,
          signInWithIdentity,
        );
        await settings
          .getByRole("button", { name: "Sign out" })
          .first()
          .click();
        await expect(settings.getByText("Signed out")).toBeVisible();
        await settings.close();

        // END-5 allows the app to keep working until the delegation it holds
        // expires, so nothing shows until one is due. It is the mint that then
        // discovers the session is gone.
        await signedInApp.focus();
        await signedInApp.ageDelegation();
        await signedInApp.replaceDelegation();

        await expect(signedInApp.state).toHaveText("no session", {
          timeout: 30_000,
        });
      },
    );

    test("picks an identity and continues", signInAsFirstIdentity);
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
    await expect(testApp.state).toHaveText("no session");

    // Another origin is another account and another session.
    await expect(other.state).toHaveText("signed in");
    await other.close();
  });

  test("a browser signed out is still the same browser", async ({
    testApp,
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

    const settings = await openSettings(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    const listed = await settings
      .getByRole("button", { name: "Sign out" })
      .count();
    await settings.getByRole("button", { name: "Sign out" }).first().click();
    await expect(settings.getByText("Signed out")).toBeVisible();

    // DEV-18: the entry stays, so signing in again reuses it.
    await testApp.focus();
    await testApp.signIn(authenticate);

    // The page showing the list shared the browser it signed out, so reading the
    // list again means signing in again — which is the same browser, and
    // therefore the same entry.
    await settings.close();
    const listedAgain = await openSettings(
      context,
      identities[0].identityNumber,
      signInWithIdentity,
    );
    await expect(
      listedAgain.getByRole("button", { name: "Sign out" }),
    ).toHaveCount(listed);
    await listedAgain.close();
  });
});
