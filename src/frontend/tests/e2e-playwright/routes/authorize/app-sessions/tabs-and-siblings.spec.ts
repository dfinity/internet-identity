import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import {
  TEST_APP_DERIVATION_ORIGIN,
  TEST_APP_SIBLING_A_URL,
  TEST_APP_SIBLING_B_URL,
} from "../../../utils";
import {
  continueAs,
  SESSION_SIGN_IN,
  SHARED_DOMAIN,
  signInAsFirstIdentity,
} from "./helpers";

/**
 * One sign-in serves every tab of an origin, and every subdomain of a domain
 * that announces it. These scenarios are about that reach: a tab or a sibling
 * that was never signed in to arrives working, and loses it when the one that
 * signed in signs out.
 *
 * Runs the "More than one tab, and sibling subdomains" scenarios of
 * `docs/ongoing/session-test-scenarios.md` — SHARE-1, SHARE-2, SHARE-3, SHARE-5
 * and SHARE-6.
 */
test.describe("more than one tab, and sibling subdomains", () => {
  test.use({ authorizeConfig: SESSION_SIGN_IN });

  test.describe("a second tab of the origin is already signed in", () => {
    test.afterEach(async ({ signedInApp, openTestApp, context }) => {
      await expect(signedInApp.account).not.toHaveText("-");
      const account = await signedInApp.account.textContent();

      const second = openTestApp(await context.newPage());
      await second.visit();

      await expect(second.state).toHaveText("signed in");
      await expect(second.account).toHaveText(account ?? "");
      await second.close();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("signing out in one tab is noticed in the other", () => {
    test.afterEach(async ({ signedInApp, openTestApp, context }) => {
      const second = openTestApp(await context.newPage());
      await second.visit();
      await expect(second.state).toHaveText("signed in");

      await signedInApp.signOut();

      await expect(second.state).toHaveText("no session");
      await second.close();
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  test.describe("closing a tab does not disturb the others", () => {
    test.afterEach(async ({ signedInApp, openTestApp, context }) => {
      const second = openTestApp(await context.newPage());
      await second.visit();
      await expect(second.state).toHaveText("signed in");
      await second.close();

      await signedInApp.replaceDelegation();
      await expect(signedInApp.state).toHaveText("signed in");
      await expect(signedInApp.delegation).not.toHaveText("none held");
    });

    test("picks an identity and continues", signInAsFirstIdentity);
  });

  // The two below configure the app per subdomain — a derivation origin and the
  // domain to announce the session across — which `authorizeConfig` has no field
  // for, so they drive the app themselves.

  test("a sibling subdomain resumes from what the domain shares", async ({
    testApp,
    openTestApp,
    context,
    identities,
    signInWithIdentity,
  }) => {
    await testApp.visit();
    await testApp.declareAlternativeOrigins([
      TEST_APP_SIBLING_A_URL,
      TEST_APP_SIBLING_B_URL,
    ]);

    await testApp.open({
      url: TEST_APP_SIBLING_A_URL,
      derivationOrigin: TEST_APP_DERIVATION_ORIGIN,
      cookieDomain: SHARED_DOMAIN,
    });
    await testApp.signIn(
      continueAs(identities[0].identityNumber, signInWithIdentity),
    );
    const account = await testApp.account.textContent();

    // HINT-1: what crosses between siblings names the account and an expiry.
    await expect(testApp.sharedHint).not.toHaveText("none");
    await expect(testApp.sharedHint).toContainText("until");

    const other = openTestApp(await context.newPage());
    await other.open({
      url: TEST_APP_SIBLING_B_URL,
      derivationOrigin: TEST_APP_DERIVATION_ORIGIN,
      cookieDomain: SHARED_DOMAIN,
    });
    // Never signed in here, but the domain announces that a session exists.
    await expect(other.sharedHint).not.toHaveText("none");
    await expect(other.state).toHaveText("no session");

    // SIL-5: it re-issues its own chain from the same session.
    await other.silentReauth();
    await expect(other.state).toHaveText("signed in", { timeout: 30_000 });
    await expect(other.account).toHaveText(account ?? "");
    await other.close();
  });

  test("signing out on one subdomain signs the siblings out", async ({
    testApp,
    openTestApp,
    context,
    identities,
    signInWithIdentity,
  }) => {
    await testApp.visit();
    await testApp.declareAlternativeOrigins([
      TEST_APP_SIBLING_A_URL,
      TEST_APP_SIBLING_B_URL,
    ]);

    await testApp.open({
      url: TEST_APP_SIBLING_A_URL,
      derivationOrigin: TEST_APP_DERIVATION_ORIGIN,
      cookieDomain: SHARED_DOMAIN,
    });
    await testApp.signIn(
      continueAs(identities[0].identityNumber, signInWithIdentity),
    );

    const other = openTestApp(await context.newPage());
    await other.open({
      url: TEST_APP_SIBLING_B_URL,
      derivationOrigin: TEST_APP_DERIVATION_ORIGIN,
      cookieDomain: SHARED_DOMAIN,
    });
    await other.silentReauth();
    await expect(other.state).toHaveText("signed in", { timeout: 30_000 });

    // HINT-3: signing out retracts what the domain shares, so the sibling has
    // nothing to resume from and asks the user instead.
    await testApp.focus();
    await testApp.signOut();
    await expect(testApp.state).toHaveText("no session");

    await other.focus();
    await other.reload();
    await expect(other.sharedHint).toHaveText("none", { timeout: 20_000 });
    await expect(other.state).toHaveText("no session");
    await other.close();
  });
});
