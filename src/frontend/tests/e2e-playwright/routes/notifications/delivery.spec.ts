import { expect, type Page } from "@playwright/test";
import { test } from "../../fixtures";
import {
  armRealWorkerPush,
  deliverPush,
  shownByTheWorker,
  startPushRelay,
} from "../../fixtures/notifications";
import {
  APP_URL,
  counted,
  expectOneMoreShown,
  someoneElseSends,
} from "../../fixtures/chatRoom";
import { II_URL } from "../../utils";
import { continueAs } from "../authorize/app-sessions/helpers";

/**
 * What happens after Internet Identity has taken a notification: the service
 * worker is woken, asks the canister what the notification is, asks the app for
 * its content, shows it, and tells the app — and tells it again when somebody
 * acts on it.
 *
 * The worker is the browser's own here, unlike the consent scenarios, and the
 * app is the test app's chatroom. Only `subscribe` is stubbed, because headless
 * Chromium has no push service; the push is delivered the way DevTools delivers
 * one, so nothing about the worker itself is simulated.
 *
 * What the worker did is read back from the app rather than from the worker:
 * the app counts a notification as received once a channel has shown it, and as
 * opened once somebody has acted on it, so the assertions are the app's own view
 * of its funnel.
 */

/** The pull delegations this browser holds, as Internet Identity's own storage. */
const delegationsHeld = (page: Page): Promise<string[]> =>
  page.evaluate(
    () =>
      new Promise<string[]>((resolve) => {
        const open = indexedDB.open("ii-notification-pull");
        open.onsuccess = () => {
          const database = open.result;
          if (!database.objectStoreNames.contains("delegations")) {
            resolve([]);
            return;
          }
          const keys = database
            .transaction("delegations")
            .objectStore("delegations")
            .getAllKeys();
          keys.onsuccess = () => resolve(keys.result.map(String));
        };
        open.onerror = () => resolve([]);
      }),
  );

/** Acting on the notification the worker has on screen, as a click would. */
const clickTheNotification = async (page: Page): Promise<void> => {
  const [worker] = page.context().serviceWorkers();
  await worker.evaluate(async () => {
    const scope = self as unknown as ServiceWorkerGlobalScope;
    const [notification] = await scope.registration.getNotifications();
    scope.dispatchEvent(
      new NotificationEvent("notificationclick", { notification }),
    );
  });
};

// The real browser rather than the headless shell: the shell answers `denied` to
// every notification permission, so nothing it is granted can ever be shown.
test.use({ channel: "chromium" });

test.describe("a notification the app sent", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  // Two wake-ups, an outcall each, and a browser doing the real work between
  // them: longer than a scenario that only clicks through screens.
  test.setTimeout(180_000);

  test("is shown by the worker and reported back to the app", async ({
    context,
    page,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const relay = await startPushRelay();
    await armRealWorkerPush(context, II_URL, relay.endpoint);
    const authenticate = continueAs(
      identities[0].identityNumber,
      signInWithIdentity,
    );

    await testApp.open({ url: APP_URL });
    await testApp.signIn(authenticate);
    await testApp.waitUntilSignedIn();
    await testApp.askToNotify(async (authPage: Page) => {
      await authenticate(authPage);
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
    });
    await testApp.expectMayNotify();

    // In the room, so the app has somebody to notify.
    await page.getByRole("button", { name: "Join", exact: true }).click();
    await expect(page.getByText("Joined as")).toBeVisible();

    // Looking at the room is reading it, and an app retracts what its reader has
    // read — so a recipient who is looking is never notified, which is the
    // behaviour, not a flaw. The page leaves the room before anyone writes to it.
    // The worker is Internet Identity's and outlives the app's page.
    await page.goto("about:blank");

    // The app keeps counting across runs, so what this scenario asks of it is
    // what it counted on top of whatever was there.
    const before = await counted();

    // Somebody else sends: a sender is never notified of their own message.
    await someoneElseSends("dinner at six?");

    // Internet Identity's own dispatch reaches the relay, and the relay standing
    // in for a push service is the only hop this test bridges by hand.
    await relay.waitForWakeUps(1);
    // Keep what the worker says to itself, so a failure names the step.
    await expect
      .poll(() => page.context().serviceWorkers().length, {
        message: "no service worker registered",
        timeout: 30_000,
      })
      .toBeGreaterThan(0);
    await deliverPush(page, II_URL);

    // The first wake-up for an app has no delegation to read the content with, so
    // it shows a placeholder and mints one — which earns the second wake-up, from
    // Internet Identity rather than from the test. Minting is what earns it, so
    // the second wake-up can be posted before the worker has finished storing
    // what it minted; a push service takes long enough that it does not, and the
    // test waits for the same reason.
    const onInternetIdentity = await context.newPage();
    await onInternetIdentity.goto(II_URL);
    await expect
      .poll(async () => (await delegationsHeld(onInternetIdentity)).length, {
        message: "the worker never minted a delegation to read the app with",
        timeout: 30_000,
      })
      .toBeGreaterThan(0);

    await relay.waitForWakeUps(2);
    await deliverPush(page, II_URL);

    await expectOneMoreShown(before);

    await expect
      .poll(
        async () =>
          (await shownByTheWorker(page.context())).map((one) => one.body),
        {
          message: "the app's own text never reached the screen",
          timeout: 15_000,
        },
      )
      .toContain("dinner at six?");

    await clickTheNotification(page);

    // At least, not exactly: the counters are the canister's own and every scenario
    // sharing this room adds to them.
    await expect
      .poll(async () => (await counted()).opened, {
        message: "the app was never told its notification was acted on",
        timeout: 30_000,
      })
      .toBeGreaterThanOrEqual(before.opened + BigInt(1));

    await relay.close();
  });
});
