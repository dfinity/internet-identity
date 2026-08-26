import { expect, type Page } from "@playwright/test";
import { test } from "../../fixtures";
import {
  II_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_DERIVATION_ORIGIN,
  TEST_APP_SIBLING_A_URL,
  TEST_APP_SIBLING_B_URL,
} from "../../utils";

/**
 * The scenarios from `docs/ongoing/session-test-scenarios.md` that a browser can
 * reach. The test app's session panel is what makes them observable: it reports
 * the account the app acts as, what the session and the app delegation have left,
 * and a log of what the library did.
 */

/** The commonest sign-in: pick an identity, then continue. */
const continueAs =
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

/** The identity's settings, signed in, with the browser list on screen. */
const openSettings = async (
  context: { newPage: () => Promise<Page> },
  identityNumber: bigint,
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>,
): Promise<Page> => {
  const settings = await context.newPage();
  await settings.goto(`${II_URL}/manage/settings`);
  await signInWithIdentity(settings, identityNumber);
  await expect(
    settings.getByRole("heading", { name: "Signed-in browsers" }),
  ).toBeVisible();
  return settings;
};

const SHARED_DOMAIN = "nice-name.com";

test.describe("app sessions", () => {
  // Every scenario here starts from a sign-in over ICRC-25, which creates the
  // session the rest of it is about. Where that is the only sign-in a scenario
  // needs, `authorizePage` performs it: the body is then the identity provider's
  // side of the ceremony, and the app is read in `afterEach`.
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test.describe("FIRST-1: a first sign-in leaves the app holding a session and a delegation", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      // The ceremony mints before it resolves, so a delegation is held straight
      // away rather than only once the tab next comes forward.
      await expect(testApp.delegation).not.toHaveText("none held");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("HOLD-1: the app keeps working for longer than one app delegation lasts", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      const account = await testApp.account.textContent();

      await testApp.ageDelegation();
      await testApp.replaceDelegation();

      await expect(testApp.state).toHaveText("signed in");
      await expect(testApp.delegation).not.toHaveText("none held");
      await expect(testApp.account).toHaveText(account ?? "");
      await expect(testApp.replacements).not.toHaveText("0");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("HOLD-2: an app left open and untouched replaces nothing", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.replacements).toHaveText("0");

      // MINT-5 and MINT-14: the scheduled refresh cancels unless the delegation
      // it would replace signed a request, so an idle tab spends nothing.
      await testApp.ageDelegation();
      await testApp.page.waitForTimeout(1000);

      await expect(testApp.replacements).toHaveText("0");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("HOLD-3: coming back to a tab replaces the delegation without being asked", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.replacements).toHaveText("0");

      // A delegation earns a replacement only once something used it (MINT-5).
      await testApp.whoAmI();

      // MINT-7: the tab coming forward is the trigger; nothing is clicked to
      // mint.
      await testApp.ageDelegation("04:55");
      await testApp.returnToTab();

      await expect(testApp.replacements).not.toHaveText("0", {
        timeout: 30_000,
      });
      await expect(testApp.state).toHaveText("signed in");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("HOLD-4: replacing the delegation renders nothing and keeps the account", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      const account = await testApp.account.textContent();
      const windowsBefore = testApp.openWindows;

      await testApp.replaceDelegation();

      // MINT-16: the account does not move. USE-6: nothing is rendered to do it.
      await expect(testApp.account).toHaveText(account ?? "");
      await expect(testApp.delegation).not.toHaveText("none held");
      expect(testApp.openWindows).toBe(windowsBefore);
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("HOLD-6: a reload comes back signed in, asking for nothing", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      const account = await testApp.account.textContent();

      await testApp.reload();

      await expect(testApp.state).toHaveText("signed in");
      await expect(testApp.account).toHaveText(account ?? "");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("SHARE-1: a second tab of the origin is already signed in", () => {
    test.afterEach(async ({ testApp, openTestApp, context }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      const account = await testApp.account.textContent();

      const second = openTestApp(await context.newPage());
      await second.visit();

      await expect(second.state).toHaveText("signed in");
      await expect(second.account).toHaveText(account ?? "");
      await second.close();
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("SHARE-2: signing out in one tab is noticed in the other", () => {
    test.afterEach(async ({ testApp, openTestApp, context }) => {
      await testApp.waitUntilSignedIn();

      const second = openTestApp(await context.newPage());
      await second.visit();
      await expect(second.state).toHaveText("signed in");

      await testApp.signOut();

      await expect(second.state).toHaveText("no session");
      await second.close();
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("SHARE-6: closing a tab does not disturb the others", () => {
    test.afterEach(async ({ testApp, openTestApp, context }) => {
      await testApp.waitUntilSignedIn();

      const second = openTestApp(await context.newPage());
      await second.visit();
      await expect(second.state).toHaveText("signed in");
      await second.close();

      await testApp.replaceDelegation();
      await expect(testApp.state).toHaveText("signed in");
      await expect(testApp.delegation).not.toHaveText("none held");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("EXIT-6: signing out leaves nothing behind, across a reload", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();

      await testApp.signOut();
      await expect(testApp.state).toHaveText("no session");

      await testApp.reload();
      await expect(testApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("a silent re-issue keeps the account and asks nothing", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await expect(testApp.account).not.toHaveText("-");
      const account = await testApp.account.textContent();

      await testApp.silentReauth();

      await expect(testApp.log).toContainText("silent re-auth", {
        timeout: 30_000,
      });
      await expect(testApp.state).toHaveText("signed in");
      await expect(testApp.account).toHaveText(account ?? "");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("a silent re-issue with nothing to answer from fails one way", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();
      await testApp.signOut();
      await expect(testApp.state).toHaveText("no session");

      await testApp.silentReauth();

      // FAIL-1 and SIL-2: one outcome, reported without asking the user
      // anything, and nothing created. Windows are not counted: the window
      // transport opens a channel either way, and SIL-1 is about screens, which
      // the redirect transport the silent design targets is what makes
      // checkable.
      await expect(testApp.log).toContainText("error", { timeout: 30_000 });
      await expect(testApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("STAY-2: clearing the site's data is a clean start", () => {
    test.afterEach(async ({ testApp }) => {
      await testApp.waitUntilSignedIn();

      await testApp.clearSiteData();
      await testApp.reload();

      await expect(testApp.state).toHaveText("no session");
    });

    test("picks an identity and continues", async ({
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      await continueAs(
        identities[0].identityNumber,
        signInWithIdentity,
      )(authorizePage.page);
    });
  });

  test.describe("through the identity's own settings", () => {
    test.describe("SHOW-2: the browser appears in the list after signing in", () => {
      test.afterEach(
        async ({ testApp, context, identities, signInWithIdentity }) => {
          await testApp.waitUntilSignedIn();
          const settings = await openSettings(
            context,
            identities[0].identityNumber,
            signInWithIdentity,
          );
          await expect(
            settings.getByRole("button", { name: "Sign out" }).first(),
          ).toBeVisible();
          await settings.close();
        },
      );

      test("picks an identity and continues", async ({
        authorizePage,
        identities,
        signInWithIdentity,
      }) => {
        await continueAs(
          identities[0].identityNumber,
          signInWithIdentity,
        )(authorizePage.page);
      });
    });

    test.describe("EXIT-3: signing the browser out from settings ends the app's access", () => {
      test.afterEach(
        async ({ testApp, context, identities, signInWithIdentity }) => {
          await testApp.waitUntilSignedIn();

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
          await testApp.focus();
          await testApp.ageDelegation();
          await testApp.replaceDelegation();

          await expect(testApp.state).toHaveText("no session", {
            timeout: 30_000,
          });
        },
      );

      test("picks an identity and continues", async ({
        authorizePage,
        identities,
        signInWithIdentity,
      }) => {
        await continueAs(
          identities[0].identityNumber,
          signInWithIdentity,
        )(authorizePage.page);
      });
    });
  });

  // The scenarios below sign in more than once, or need the app configured in a
  // way `authorizeConfig` does not describe, so they drive the app themselves.

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

  test("FIRST-3: signing in twice from one browser replaces the session rather than adding one", async ({
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

  test("EXIT-1: signing out of one app leaves the other alone", async ({
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

  test("STAY-3: an interrupted sign-in can be retried", async ({
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

    test("STAY-4: two identities in one browser stay apart", async ({
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

  test.describe("across the siblings of a domain", () => {
    test("SHARE-3: a sibling resumes from what the domain shares", async ({
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

    test("SHARE-5: signing out on one subdomain signs the siblings out", async ({
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

  test.describe("more sign-ins than the identity keeps", () => {
    test("EXIT-5: a browser signed out is still the same browser", async ({
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

      // The page showing the list shared the browser it signed out, so reading
      // the list again means signing in again — which is the same browser, and
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

    test("CAP-6: the list keeps the recent browsers and drops the rest", async ({
      browser,
      openTestApp,
      identities,
      signInWithIdentity,
    }) => {
      test.setTimeout(15 * 60 * 1000);
      // A context is a separate browser as far as the identity is concerned, so
      // the 20-entry cap is reachable without 20 machines.
      const CAP = 20;
      const browsers = [];
      for (let index = 0; index < CAP + 2; index++) {
        const fresh = await browser.newContext({ ignoreHTTPSErrors: true });
        const app = openTestApp(await fresh.newPage());
        await app.open();
        await app.signIn(
          continueAs(identities[0].identityNumber, signInWithIdentity),
        );
        browsers.push({ fresh, app });
      }

      // DEV-14: reaching the limit drops the least recently used rather than
      // refusing, so the newest sign-in worked and the list is capped.
      const settings = await openSettings(
        browsers[browsers.length - 1].fresh,
        identities[0].identityNumber,
        signInWithIdentity,
      );
      const listed = await settings
        .getByRole("button", { name: "Sign out" })
        .count();
      expect(listed).toBeLessThanOrEqual(CAP);

      // DEV-15: dropping an entry ends that browser's sessions.
      const oldest = browsers[0].app;
      await oldest.focus();
      await oldest.ageDelegation();
      await oldest.replaceDelegation();
      await expect(oldest.state).toHaveText("no session", { timeout: 30_000 });

      for (const { fresh } of browsers) await fresh.close();
    });
  });
});
