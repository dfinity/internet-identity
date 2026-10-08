import type { Page } from "@playwright/test";
import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  armPushNotifications,
  liftPermissionInSettings,
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
   * A refusal is asked about like any other, so the user sees what they are being
   * asked before being sent to settings. The browser answers without prompting, and
   * the guidance is what that answer leads to.
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
      await authPage.getByRole("button", { name: "Allow" }).click();

      await expect(
        authPage.getByRole("heading", { name: "Notifications are blocked" }),
      ).toBeVisible();
      // The agent here is Chromium's, whose route starts at the address bar.
      await expect(
        authPage.getByRole("heading", { name: "Open site settings" }),
      ).toBeVisible();
      // Asked once. The browser answers a refusal without showing anything, and
      // nothing on the guidance asks again.
      expect(await promptCount(authPage)).toBe(1);
      await authPage.getByRole("button", { name: "Not now" }).click();
    });

    await testApp.expectMayNotNotify();
  });

  /** The guidance has no way back: nothing in the page can lift a refusal, so the
   *  screen watches the permission and finishes what the user started. */
  test("a permission lifted in settings carries on without being pressed", async ({
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
      await authPage.getByRole("button", { name: "Allow" }).click();
      await expect(
        authPage.getByRole("heading", { name: "Notifications are blocked" }),
      ).toBeVisible();

      // Nothing is pressed after this: the window closing is the flow finishing.
      await liftPermissionInSettings(authPage);
    });

    await testApp.expectMayNotify();
  });

  /**
   * Firefox's route is to clear the block, which returns the permission to `default`
   * rather than granting it. A prompt can be raised again, but only off a gesture,
   * so the user lands back on the ask instead of on a prompt they did not ask for.
   */
  test("a block cleared rather than granted lands back on the ask", async ({
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
      await authPage.getByRole("button", { name: "Allow" }).click();
      await expect(
        authPage.getByRole("heading", { name: "Notifications are blocked" }),
      ).toBeVisible();

      await liftPermissionInSettings(authPage, "default");

      // Back on the ask, with a button that now works, rather than stranded on the
      // guidance with only "Not now".
      await expect(
        authPage.getByRole("button", { name: "Allow" }),
      ).toBeVisible();
      await authPage.getByRole("button", { name: "Allow" }).click();
    });

    await testApp.expectMayNotify();
  });

  /**
   * An app that asks to notify as part of signing in sends both requests at once, and
   * the consent request can reach Internet Identity first. The sign-in still has to run
   * first: it carries the app's session duration, and it registers this browser, which
   * allowing needs. Each scenario starts on a browser that has never signed in.
   */
  test.describe("asked together with the sign-in", () => {
    const ONE_HOUR_MS = 3_600_000;
    const ONE_HOUR_NS = BigInt(ONE_HOUR_MS) * BigInt(1_000_000);

    test("an app is told yes on a browser signing in for the first time", async ({
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

      await testApp.open({ askToNotifyOnSignIn: true });
      await testApp.signIn(async (authPage: Page) => {
        await authenticate(authPage);
        await authPage
          .getByRole("button", { name: "Allow", exact: true })
          .click();
      });

      await testApp.expectMayNotify();
    });

    test("the session lasts no longer than the app asked for", async ({
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

      await testApp.open({
        askToNotifyOnSignIn: true,
        maxTimeToLive: ONE_HOUR_NS,
      });
      await testApp.signIn(async (authPage: Page) => {
        await authenticate(authPage);
        await authPage.getByRole("button", { name: "Not now" }).click();
      });

      await testApp.expectSessionEndsWithin(ONE_HOUR_MS);
    });

    test("an app the user puts off is told no and still signed in", async ({
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

      await testApp.open({ askToNotifyOnSignIn: true });
      await testApp.signIn(async (authPage: Page) => {
        await authenticate(authPage);
        await authPage.getByRole("button", { name: "Not now" }).click();
      });

      await testApp.expectMayNotNotify();
    });
  });
});
