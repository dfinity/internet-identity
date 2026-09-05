import { test } from "../../../fixtures";
import { signInAsFirstIdentity } from "./helpers";

/**
 * An app delegation lasts five minutes and a session lasts hours, so an app left
 * open outlives what it was handed. These are the scenarios about that gap being
 * invisible: replacements happen when they are needed, cost nothing when they are
 * not, and never put a screen in front of the user.
 *
 * Runs the "Staying signed in" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — HOLD-1, HOLD-2, HOLD-3, HOLD-4 and
 * HOLD-6 — and the silent re-issue an app can ask for itself.
 */
test.describe("staying signed in", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test.describe("the app keeps working for longer than one app delegation lasts", () => {
    test.afterEach(async ({ signedInApp }) => {
      const account = await signedInApp.accountPrincipal();

      await signedInApp.ageDelegation();
      await signedInApp.replaceDelegation();

      await signedInApp.waitUntilSignedIn();
      await signedInApp.expectHoldsDelegation();
      await signedInApp.expectAccount(account);
      await signedInApp.expectDelegationReplaced();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("an app left open and untouched replaces nothing", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.expectNoDelegationReplacements();

      // MINT-5 and MINT-14: the scheduled refresh cancels unless the delegation
      // it would replace signed a request, so an idle tab spends nothing.
      await signedInApp.ageDelegation();
      await signedInApp.page.waitForTimeout(1000);

      await signedInApp.expectNoDelegationReplacements();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("coming back to a tab replaces the delegation without being asked", () => {
    test.afterEach(async ({ signedInApp }) => {
      await signedInApp.expectNoDelegationReplacements();

      // A delegation earns a replacement only once something used it (MINT-5).
      await signedInApp.whoAmI();

      // MINT-7: the tab coming forward is the trigger; nothing is clicked to
      // mint.
      await signedInApp.ageDelegation("04:55");
      await signedInApp.returnToTab();

      await signedInApp.expectDelegationReplaced();
      await signedInApp.waitUntilSignedIn();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("replacing the delegation renders nothing and keeps the account", () => {
    test.afterEach(async ({ signedInApp }) => {
      const account = await signedInApp.accountPrincipal();

      // USE-6: nothing is rendered to replace a delegation.
      await signedInApp.expectNothingOpens(() =>
        signedInApp.replaceDelegation(),
      );

      // MINT-16: the account does not move.
      await signedInApp.expectAccount(account);
      await signedInApp.expectHoldsDelegation();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("a reload comes back signed in, asking for nothing", () => {
    test.afterEach(async ({ signedInApp }) => {
      const account = await signedInApp.accountPrincipal();

      await signedInApp.reload();

      await signedInApp.waitUntilSignedIn();
      await signedInApp.expectAccount(account);
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("a silent re-issue keeps the account and asks nothing", () => {
    test.afterEach(async ({ signedInApp }) => {
      const account = await signedInApp.accountPrincipal();

      await signedInApp.silentReauth();

      await signedInApp.expectSilentReauthSucceeded();
      await signedInApp.expectAccount(account);
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });
});
