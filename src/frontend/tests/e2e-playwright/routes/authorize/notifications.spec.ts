import type { Page } from "@playwright/test";
import { test } from "../../fixtures";
import { armPushNotifications } from "../../fixtures/notifications";
import { continueAs } from "./app-sessions/helpers";

/**
 * An app asking Internet Identity whether it may notify one of its users.
 *
 * The request is a method of its own rather than part of signing in, so every scenario
 * signs in first and then asks, and the second provider window is the point.
 *
 * Only the browser's push service is stubbed, since headless Chromium has no real one.
 * The consent screen, the VAPID pool the browser signs and the rows the canister writes
 * all run for real, and what the app is told is read back from the canister.
 */
test.describe("notification consent", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test("an app that asks is told yes once the user allows it", async ({
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
        .getByRole("button", { name: "Enable notifications" })
        .click();
    });

    await testApp.expectMayNotify();
  });

  test("an app that asks is told no when the user puts it off", async ({
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
      await authPage.getByRole("button", { name: "Maybe later" }).click();
    });

    // Declining is an answer, not a failure: the app is told it may not notify
    // rather than left waiting on a request that never resolves.
    await testApp.expectMayNotNotify();
  });

  test("a browser whose permission was refused is not prompted again", async ({
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    // A refusal stands until the user changes it in browser settings, which no prompt
    // can do, so II offers guidance instead of a button that cannot work.
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
      await authPage.getByRole("button", { name: "Continue without" }).click();
    });

    await testApp.expectMayNotNotify();
  });
});
