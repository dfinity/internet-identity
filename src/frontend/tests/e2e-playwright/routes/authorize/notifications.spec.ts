import type { Page } from "@playwright/test";
import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  armPushNotifications,
  promptCount,
} from "../../fixtures/notifications";
import { continueAs } from "./app-sessions/helpers";

/**
 * An app asking Internet Identity whether it may notify one of its users.
 *
 * The request is a method of its own rather than part of signing in, so every scenario
 * signs in first and then asks, and the second provider window is the point.
 *
 * The browser's notification and service-worker APIs are stubbed, since headless
 * Chromium has no push service behind them, so no scenario here covers the native
 * permission prompt or a real registration. The consent screen, the VAPID pool the
 * browser signs and the rows the canister writes all run for real, and what the app is
 * told is read back from the canister.
 *
 * What "yes" means is both halves: this identity allowed the app, and this browser
 * delivers for it. Every scenario below is one shape of that.
 */
test.describe("notification consent", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test("an app is told yes once this browser can deliver for it", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
    });

    await testApp.expectMayNotify();
  });

  /** One screen asks whatever is already set up, so this is the same Allow that a
   *  first-timer presses: what it runs underneath is the part that differs. */
  test("an app is told no when the user puts it off", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage.getByRole("button", { name: "Not now" }).click();
    });

    // Declining is an answer, not a failure: the app is told it may not notify
    // rather than left waiting on a request that never resolves.
    await testApp.expectMayNotNotify();
  });

  /**
   * "Not now" ends this request and nothing more.
   *
   * It used to quiet the app for a fortnight, which meant a user who meant "not
   * right now" was not asked again for two weeks, and an app that asked got an
   * answer no one had been shown. Remembering a no is the app's job.
   */
  test("an app that asks again after a refusal is asked again", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage.getByRole("button", { name: "Not now" }).click();
    });
    await testApp.expectMayNotNotify();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      // Asked again rather than answered from what the last one said.
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
    });

    await testApp.expectMayNotify();
  });

  /**
   * The screen most users never see.
   *
   * Once this identity allows the app and this browser is registered for it, there is
   * nothing to ask, so the request is answered without anything being put in front of
   * the user. This is the common path on every sign-in after the first, and the reason
   * the question is resolved before a screen is opened rather than after.
   */
  test("an app already allowed on a set-up browser is answered with no screen", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
    });
    await testApp.expectMayNotify();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      // Nothing is asked, so nothing to answer: no button and no prompt.
      await expect(
        authPage.getByRole("button", { name: "Allow", exact: true }),
      ).toHaveCount(0);
      expect(await promptCount(authPage)).toBe(0);
    });

    await testApp.expectMayNotify();
  });

  /**
   * A refusal stands until the user lifts it in browser settings, which no prompt can
   * do. So this screen raises none, and shows the route through this browser's own
   * settings instead of a button that cannot work.
   */
  test("a browser whose permission was refused is shown how to unblock", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    await armPushNotifications(context, { permission: "denied" });
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open();
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();

    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await expect(
        authPage.getByRole("heading", { name: "Notifications are blocked" }),
      ).toBeVisible();
      // The agent here is Chromium's, whose route starts at the address bar.
      await expect(
        authPage.getByRole("heading", { name: "Open site settings" }),
      ).toBeVisible();
      expect(await promptCount(authPage)).toBe(0);
      await authPage.getByRole("button", { name: "Not now" }).click();
    });

    await testApp.expectMayNotNotify();
  });
});
