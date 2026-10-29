import {
  expect,
  type Browser,
  type BrowserContext,
  type Page,
} from "@playwright/test";
import { test } from "../../fixtures";
import {
  armRealWorkerPush,
  deliverPush,
  shownByTheWorker,
  startPushRelay,
  type PushRelay,
} from "../../fixtures/notifications";
import {
  addToHomeScreen,
  askToNotify,
  reportShown,
  expectSetUp,
  IPHONE_AGENT,
  type HomeScreenApp,
} from "../../fixtures/homeScreenApp";
import type { TestApp } from "../../fixtures/testApp";
import {
  APP_URL,
  counted,
  expectOneMoreShown,
  someoneElseSends,
} from "../../fixtures/chatRoom";
import { II_URL } from "../../utils";
import { continueAs } from "../authorize/app-sessions/helpers";

/**
 * Notifications on an iPhone, the one platform where they do not arrive in the browser.
 *
 * Web push exists there only inside a web app added to the Home Screen, and iOS gives
 * that app a storage partition of its own: it cannot read the browser's keys and the
 * browser cannot read its subscription. So this runs two browser contexts, which is what
 * separate storage is, and the only thing crossing between them is what the canister
 * knows.
 *
 * Nothing about the flow is simulated. The same route serves both documents, the app's
 * own service worker takes the push, and the canister is told and asked for real. What
 * the test stands in for is the push service, which headless Chromium has none of, and
 * the one answer only a push service can give: that a subscription is gone.
 */

// A phone, not a desktop: what Internet Identity offers is decided from the user agent.
test.use({
  channel: "chromium",
  userAgent: IPHONE_AGENT,
  viewport: { width: 393, height: 852 },
  isMobile: true,
  hasTouch: true,
  authorizeConfig: { protocol: "icrc25" },
});

/** Answering the opt-in, which on a phone hands the user the install rather than a
 *  prompt this browser could not act on. */
const takeTheInstall = async (authPage: Page): Promise<string> => {
  const opened = authPage.context().waitForEvent("page");
  await authPage.getByRole("button", { name: "Allow", exact: true }).click();
  const installPage = await opened;
  await installPage.waitForLoadState();
  await expect(
    installPage.getByRole("heading", { name: "Set up ICP Alerts" }),
  ).toBeVisible();
  const url = installPage.url();
  await installPage.close();
  // Nothing more to press: the steps are walked in the tab that just opened, and the
  // page that opened it settles sign-in behind them once it has pointed that tab.
  return url;
};

/**
 * The install, from a browser holding nothing to an app on the Home Screen.
 *
 * Where every scenario here starts, and the same steps in each: a relay standing in for
 * the push service, a sign-in at the test app, the opt-in that hands back the install
 * URL, and the app added from that URL.
 */
const installOnThePhone = async ({
  browser,
  context,
  testApp,
  identityNumber,
  signInWithIdentity,
}: {
  browser: Browser;
  context: BrowserContext;
  testApp: TestApp;
  identityNumber: bigint;
  signInWithIdentity: (page: Page, identityNumber: bigint) => Promise<void>;
}): Promise<{
  relay: PushRelay;
  app: HomeScreenApp;
  installUrl: string;
}> => {
  const relay = await startPushRelay();
  await armRealWorkerPush(context, II_URL, relay.endpoint);
  const authenticate = continueAs(identityNumber, signInWithIdentity);

  await testApp.open({ url: APP_URL });
  await testApp.signIn(authenticate);
  await testApp.waitUntilSignedIn();

  let installUrl = "";
  await askToNotify(testApp.page, async (authPage: Page) => {
    await authenticate(authPage);
    installUrl = await takeTheInstall(authPage);
  });

  // Adding it to the Home Screen, which is a partition of its own.
  const app = await addToHomeScreen(browser, installUrl, relay.endpoint);
  await expectSetUp(app);
  return { relay, app, installUrl };
};

/** In the room, so the app has somebody to notify, and then away from it: a reader who
 *  is looking is never notified. */
const joinTheRoomAndLeave = async (page: Page): Promise<void> => {
  await page.goto(APP_URL);
  await page.getByRole("button", { name: "Join", exact: true }).click();
  await expect(page.getByText("Joined as")).toBeVisible();
  await page.goto("about:blank");
};

/**
 * Waits for the app's own text, delivering wake-ups until it arrives.
 *
 * How many wake-ups that takes is not fixed. A mint that answers inside the worker's
 * deadline shows the text on the wake-up that asked for it; one that overruns spends a
 * placeholder, and what replaces it is the wake-up the mint earned. Only a delivery
 * brings that one in, because the relay counts wake-ups rather than delivering them, so
 * waiting without delivering waits on a placeholder nothing can replace.
 *
 * A wake-up with nothing left to show is harmless here: the worker places a placeholder
 * only where the screen is empty, and by this point it is not.
 */
const expectTheAppToShow = async (
  app: HomeScreenApp,
  relay: PushRelay,
  /** Wake-ups already handed to the worker, so each delivery here spends one the
   *  canister actually posted. */
  bridged: number,
  body: string,
  message: string,
): Promise<void> => {
  let spent = bridged;
  await expect
    .poll(
      async () => {
        const shown = (await shownByTheWorker(app.context)).map(
          (one) => one.body,
        );
        // A push the canister never posted is one a phone would never receive. The relay
        // stands in for the push service, so only what reached it may be delivered, and
        // a canister that stopped posting has to fail this rather than be covered for.
        if (!shown.includes(body) && relay.received() > spent) {
          spent += 1;
          await deliverPush(app.page, II_URL);
        }
        return shown;
      },
      { message, timeout: 60_000 },
    )
    .toContain(body);
};

/**
 * Lets the app answer the probe the switch sends when it is turned on.
 *
 * Turning delivery on asks the app to prove it is still on the device, and only the app
 * moving its own stamp answers. On a phone the push arrives by itself; here the relay
 * holds wake-ups until a test delivers them, so a probe nobody delivers goes unanswered
 * and the switch offers an install for an app that is sitting right there.
 */
const answerTheProbe = async (
  app: HomeScreenApp,
  relay: PushRelay,
  sentBefore: number,
): Promise<void> => {
  await relay.waitForWakeUps(sentBefore + 1);
  await deliverPush(app.page, II_URL);
};

/**
 * Somebody writes to the room, and the app ends up showing it.
 *
 * The wake-ups are counted from just before the send, because the relay counts every
 * wake-up the canister ever posted and pointing an identity at the app posts one of its
 * own before any of this.
 *
 * Only the first wake-up is waited for. How many a notification takes is not fixed: a
 * mint that answers in time shows the app's own text on this one, and one that overruns
 * spends a placeholder and earns another, which `expectTheAppToShow` delivers. Waiting
 * for a second here would wait for one the canister never posts.
 */
const expectDelivery = async (
  app: HomeScreenApp,
  relay: PushRelay,
  text: string,
  message: string,
): Promise<void> => {
  const sentSoFar = relay.received();
  await someoneElseSends(text);

  await relay.waitForWakeUps(sentSoFar + 1);
  await expect
    .poll(() => app.context.serviceWorkers().length, {
      message: "the app registered no service worker",
      timeout: 30_000,
    })
    .toBeGreaterThan(0);
  await deliverPush(app.page, II_URL);
  // Reporting, not asserting: it puts what is on screen in the output so a failure below
  // says whether the worker never ran or ran and could not read the app's text.
  await reportShown(app, "after the first wake-up");

  await expectTheAppToShow(app, relay, sentSoFar + 1, text, message);
};

/**
 * The same install URL carrying a link key of its own: one no browser registered, so
 * the canister holds nothing pending for the principal the app signs the link with.
 *
 * Still a well-formed token, so the launch decodes it, builds an actor from it and
 * spends it: what is being exercised is the refusal, not the parsing. The layout is the
 * one `exportLinkKey` writes, the private scalar and both halves of the point.
 */
const withAKeyNothingIsWaitingFor = async (
  installUrl: string,
): Promise<string> => {
  const { privateKey } = (await crypto.subtle.generateKey(
    { name: "ECDSA", namedCurve: "P-256" },
    true,
    ["sign", "verify"],
  )) as CryptoKeyPair;
  const { d, x, y } = await crypto.subtle.exportKey("jwk", privateKey);
  const url = new URL(installUrl);
  const params = new URLSearchParams(url.hash.replace(/^#/, ""));
  params.set(
    "key",
    Buffer.concat(
      [d, x, y].map((coordinate) => Buffer.from(coordinate ?? "", "base64url")),
    ).toString("base64url"),
  );
  url.hash = params.toString();
  return url.toString();
};

/**
 * Counts the queries this tab sends the canister, which is what the watcher behind the
 * install steps spends its time doing.
 *
 * Counted rather than waited out: what the steps have to outlast is the watcher asking
 * and asking again, and a count of asks says that where a stretch of wall clock only
 * hopes for it.
 */
const queriesSent = (page: Page): (() => number) => {
  let queries = 0;
  page.on("request", (request) => {
    if (/\/api\/v\d+\/canister\/[^/]+\/query$/.test(request.url())) {
      queries += 1;
    }
  });
  return () => queries;
};

/** How many asks the steps have to survive to prove they are not settling against the
 *  record the deleted app left behind. The watcher asks every three seconds. */
const QUERIES_TO_OUTLAST = 5;

/** The canister drains its notification backlog on a one second timer, so a pass has
 *  certainly run by the time this is up. */
const DISPATCH_PASSES_WITHIN_MS = 5_000;

test.describe("notifications on a Home Screen app", () => {
  // Two contexts, a service worker, and outcalls between them.
  test.setTimeout(300_000);

  test("are set up by installing the app, and arrive in it", async ({
    browser,
    context,
    page,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app, installUrl } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    // The token rides the fragment, which is what iOS captures into the Home Screen
    // entry and hands back at every launch.
    expect(installUrl).toContain("/ios-icp-alerts#");

    await joinTheRoomAndLeave(page);

    const before = await counted();
    await expectDelivery(
      app,
      relay,
      "dinner at six?",
      "the app's own text never reached the Home Screen app",
    );

    await expectOneMoreShown(before);

    await app.delete();
    await relay.close();
  });

  test.describe("with a second identity on the phone", () => {
    // The only scenario that needs two, so the second is created here and not for the
    // five that sign in once.
    test.use({
      identityConfig: {
        createIdentities: [{ name: "Test 1" }, { name: "Test 2" }],
      },
    });

    test("reach a second identity through the app the first one installed", async ({
      browser,
      context,
      page,
      testApp,
      identities,
      signInWithIdentity,
    }) => {
      // The first identity installs it.
      const { relay, app } = await installOnThePhone({
        browser,
        context,
        testApp,
        identityNumber: identities[0].identityNumber,
        signInWithIdentity,
      });

      // The second signs in on the same phone. The app is one thing on the device, so it
      // is pointed at rather than installed again: nothing opens, and the identity is
      // allowed to notify straight away.
      await testApp.signOut();
      const second = continueAs(
        identities[1].identityNumber,
        signInWithIdentity,
      );
      await testApp.signIn(second);
      await testApp.waitUntilSignedIn();

      let sentToInstall = false;
      let stopWatching = (): void => {};
      await askToNotify(testApp.page, async (authPage: Page) => {
        await second(authPage);
        // Armed around the click and nothing else: the authorize window this ceremony
        // opened is a page too, and the install window a second identity should not be
        // sent to would open from the click rather than with it.
        const watched = authPage.context();
        const opened = (): void => {
          sentToInstall = true;
        };
        watched.on("page", opened);
        stopWatching = () => watched.off("page", opened);
        await authPage
          .getByRole("button", { name: "Allow", exact: true })
          .click();
      });
      stopWatching();
      await testApp.expectMayNotify();
      expect(
        sentToInstall,
        "the second identity was sent to install an app the phone already has",
      ).toBe(false);

      await joinTheRoomAndLeave(page);

      await expectDelivery(
        app,
        relay,
        "second identity, same phone",
        "the second identity's notification never arrived",
      );

      await app.delete();
      await relay.close();
    });
  });

  test("offer the install again once the app has been deleted", async ({
    browser,
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app, installUrl } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    // The user deletes it from the Home Screen: the partition goes, the key with it,
    // and nothing tells the canister. The push service is the only party that finds
    // out, and it says so by refusing the next wake-up.
    await app.delete();
    const beforeTheProbe = relay.received();
    relay.reportGone(true);

    // Back in the browser, on the page that decides what to offer. It wakes the app to
    // see whether it is still there, and what comes back is a dead subscription.
    const installPage = await context.newPage();
    await installPage.goto(installUrl);

    await expect(
      installPage.getByRole("heading", { name: "Set up ICP Alerts" }),
    ).toBeVisible({ timeout: 60_000 });

    // The one-shot key in the URL this tab arrived on was spent by the app that has
    // gone, so the steps have to be offered on a freshly minted one: an entry added
    // from the old URL could never link.
    expect(
      installPage.url(),
      "the steps are offered on the key the deleted app already spent",
    ).not.toBe(installUrl);

    // The steps are also what a browser pointing at nothing is shown, so the screen on
    // its own does not say which of the two got here. A wake-up does: a probe that found
    // no record posts nothing at all, so one arriving at the app's endpoint while the
    // relay is answering 410 is the refusal this scenario is about.
    expect(
      relay.receivedPaths().slice(beforeTheProbe),
      "no wake-up was posted, so nothing was ever refused",
    ).toContain(new URL(relay.endpoint).pathname);

    // And the record is gone, which only the refusal can do. The relay answers 201 again,
    // so a record that survived would put a fresh tab back on the ready screen; the steps
    // are what is left when there is no record to read.
    relay.reportGone(false);
    const afterTheCleanup = await context.newPage();
    await afterTheCleanup.goto(installUrl);
    await expect(
      afterTheCleanup.getByRole("heading", { name: "Set up ICP Alerts" }),
    ).toBeVisible({ timeout: 60_000 });
    await expect(
      afterTheCleanup.getByRole("heading", { name: "You're all set" }),
    ).toBeHidden();

    await relay.close();
  });

  test("come back after being turned off and on again", async ({
    browser,
    context,
    page,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    await joinTheRoomAndLeave(page);

    // Turned off where the user turns it off, which is this identity's own switch and
    // never the app's subscription: the app keeps delivering for anyone else.
    // The settings page first and the sign-in second: the session lives in memory, so
    // navigating after signing in drops it and lands back on the picker.
    const settings = await context.newPage();
    await settings.goto(`${II_URL}/manage/settings`);
    await signInWithIdentity(settings, identities[0].identityNumber);
    const notifications = settings.getByRole("switch", {
      name: "Allow notifications",
    });
    await expect(notifications).toBeVisible({ timeout: 30_000 });
    await notifications.click();
    // The switch answers the press at once and writes afterwards, so the canister has
    // the flag only once the press has let go of the control.
    await expect(notifications).toBeEnabled({ timeout: 30_000 });
    await expect(notifications).not.toBeChecked({ timeout: 30_000 });

    // A message while it is off. The canister skips a reference the identity turned off
    // when it picks who to send to, so nothing is dispatched for this one at all.
    const before = await counted();
    await someoneElseSends("while it was off");

    // The pass that drops it has to have run while the switch was still off, because the
    // reference is read when the backlog is drained rather than when the message arrives.
    // That pass is on a timer and nothing it does here is observable from outside: it
    // wakes nobody and posts nothing, so there is no signal to poll and the wait is keyed
    // to the dispatch interval instead. Turning the switch back on first would leave the
    // message in the backlog for a pass that now finds the identity listening, which is a
    // race this test lost rather than behaviour it means to pin.
    await settings.waitForTimeout(DISPATCH_PASSES_WITHIN_MS);

    // What the switch promises is that the message never reaches the screen, and that is
    // what is asserted. The relay's count cannot say it: it counts every wake-up posted
    // to this endpoint, and an entry with anything still queued is woken again on a timer,
    // so a slow worker that has not yet fetched an earlier notification raises the count
    // for a reason that has nothing to do with the message being silenced.
    expect(
      (await shownByTheWorker(app.context)).map((one) => one.body),
      "the message that arrived while the switch was off was shown anyway",
    ).not.toContain("while it was off");

    // And on again, which needs no install: the app is still there and still registered.
    // The press asks it to say so, and the app answering is what keeps the switch on.
    const beforeTheProbe = relay.received();
    await notifications.click();
    await answerTheProbe(app, relay, beforeTheProbe);
    await expect(notifications).toBeEnabled({ timeout: 30_000 });
    await expect(notifications).toBeChecked({ timeout: 30_000 });

    await expectDelivery(
      app,
      relay,
      "switched back on",
      "nothing arrived after the switch went back on",
    );

    // Asked once the later message is on screen, so the earlier one has had every chance
    // this one had. One of the two was shown, and the app counts which.
    expect(
      (await shownByTheWorker(app.context)).map((one) => one.body),
      "the message that arrived while the switch was off was delivered anyway",
    ).not.toContain("while it was off");
    await expectOneMoreShown(before);

    await app.delete();
    await relay.close();
  });

  test("offer the install from settings once the app has been deleted", async ({
    browser,
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    // Deleted from the Home Screen, and nothing tells the canister: the relay answers
    // for that endpoint exactly as before, so the record goes on saying it delivers.
    // That record is what the switch used to believe.
    await app.delete();

    const settings = await context.newPage();
    await settings.goto(`${II_URL}/manage/settings`);
    await signInWithIdentity(settings, identities[0].identityNumber);
    const notifications = settings.getByRole("switch", {
      name: "Allow notifications",
    });
    await expect(notifications).toBeVisible({ timeout: 30_000 });

    await notifications.click();
    await expect(notifications).toBeEnabled({ timeout: 30_000 });
    await expect(notifications).not.toBeChecked({ timeout: 30_000 });

    // On again is where it used to end: the entry still said subscribed, so the switch
    // reported delivery and offered nothing. Now the press asks the app to prove it is
    // there, nothing answers, and the install is what is left.
    const steps = settings.context().waitForEvent("page", { timeout: 60_000 });
    await notifications.click();
    const offered = await steps;
    await expect(
      offered.getByRole("heading", { name: "Set up ICP Alerts" }),
    ).toBeVisible({ timeout: 60_000 });

    await relay.close();
  });

  /**
   * Signing in to an app that asks for notifications, on a phone whose Home Screen app
   * has been deleted and whose switch is off.
   *
   * `subscribed` outlives the app it names, so this used to report success and hand
   * sign-in straight back: the dapp was told it may notify a device with nothing to
   * notify, and the steps were never offered. The press now asks the app to answer for
   * itself, and the steps are what is left when it does not.
   */
  test("offer the install from sign-in once the app has been deleted", async ({
    browser,
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    // Switched off where the user switches it off. The proof only runs for a switch that
    // is off, since an app already delivering has nothing to prove and a probe for every
    // consent would be a notification nobody asked for.
    const settings = await context.newPage();
    await settings.goto(`${II_URL}/manage/settings`);
    await signInWithIdentity(settings, identities[0].identityNumber);
    const notifications = settings.getByRole("switch", {
      name: "Allow notifications",
    });
    await expect(notifications).toBeVisible({ timeout: 30_000 });
    await notifications.click();
    await expect(notifications).toBeEnabled({ timeout: 30_000 });
    await expect(notifications).not.toBeChecked({ timeout: 30_000 });

    // Deleted from the Home Screen, which tells the canister nothing: the relay answers
    // for that endpoint exactly as before, so the record goes on saying it delivers.
    await app.delete();

    // Back to the app, which asks again. Nothing is delivered by hand from here, so the
    // probe this press sends goes unanswered and times out, which is the app being gone.
    await testApp.signOut();
    const again = continueAs(identities[0].identityNumber, signInWithIdentity);
    await testApp.signIn(again);
    await testApp.waitUntilSignedIn();

    await askToNotify(testApp.page, async (authPage: Page) => {
      await again(authPage);
      const allow = authPage.getByRole("button", {
        name: "Allow",
        exact: true,
      });

      // The first press is what discovers the app is gone: it asks the app to answer,
      // the probe goes unanswered, and the screen becomes the one that installs. The
      // button says so while it waits, and comes back when the answer is in.
      await allow.click();
      await expect(
        authPage.getByRole("button", { name: /Setting up/ }),
      ).toBeVisible({ timeout: 30_000 });
      await expect(allow).toBeVisible({ timeout: 60_000 });

      // Armed around the second press: the tab is opened inside the gesture, before
      // anything is awaited, which is the only time a browser allows one.
      const steps = authPage
        .context()
        .waitForEvent("page", { timeout: 60_000 });
      await allow.click();

      // The tab reaches the steps rather than closing on an install that was refused:
      // an ordinary one is refused over a record that still says it delivers, so the
      // replacement is what this press has to mint. Reaching them also proves the tab
      // was pointed before sign-in handed back and took the page that points it.
      const offered = await steps;
      await expect(
        offered.getByRole("heading", { name: "Set up ICP Alerts" }),
      ).toBeVisible({ timeout: 60_000 });
      await offered.close();
    });

    // Sign-in still settles, with nothing left on screen to press: a second screen
    // between the press and the handback would leave this waiting for ever. What the app
    // is told is read from the canister rather than from the press, and it is told this
    // identity allowed it *and* this browser delivers. The consent is recorded, but the
    // steps have only been offered, so until they are walked this device delivers
    // nothing and the answer is no.
    await testApp.expectMayNotNotify();

    // An entry that delivers for nobody must not read as delivering once the steps are
    // offered rather than finished. Read on a page of its own: the session lives in
    // memory, so reloading the one from earlier drops it and lands on the picker.
    const afterwards = await context.newPage();
    await afterwards.goto(`${II_URL}/manage/settings`);
    await signInWithIdentity(afterwards, identities[0].identityNumber);
    const stillOff = afterwards.getByRole("switch", {
      name: "Allow notifications",
    });
    await expect(stillOff).toBeVisible({ timeout: 30_000 });
    await expect(stillOff).not.toBeChecked({ timeout: 30_000 });

    await relay.close();
  });

  test("keep the install steps up until a new app delivers", async ({
    browser,
    context,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    // Deleted from the Home Screen, and the push service never told. The record it
    // leaves behind still says subscribed and enabled, and goes on saying so: nothing
    // bounces, because the relay answers for that endpoint exactly as before.
    await app.delete();

    // Back in the browser, in settings, which is where a device that lost its app says
    // so. The tab the install was walked in only ever reports what the record says.
    const settings = await context.newPage();
    await settings.goto(`${II_URL}/manage/settings`);
    await signInWithIdentity(settings, identities[0].identityNumber);
    const notifications = settings.getByRole("switch", {
      name: "Allow notifications",
    });
    await expect(notifications).toBeVisible({ timeout: 30_000 });
    await notifications.click();
    await expect(notifications).toBeEnabled({ timeout: 30_000 });
    await expect(notifications).not.toBeChecked({ timeout: 30_000 });

    // The press asks the app to prove it is there. Nothing answers, so the steps are
    // offered in a tab of their own.
    const steps = settings.context().waitForEvent("page", { timeout: 60_000 });
    await notifications.click();
    const installPage = await steps;
    const asked = queriesSent(installPage);
    await expect(
      installPage.getByRole("heading", { name: "Set up ICP Alerts" }),
    ).toBeVisible({ timeout: 60_000 });

    // The window this scenario exists for. The old record is still there, still
    // subscribed and still enabled, so a watcher that settled against it would call the
    // install done while the user is on the first step. Nothing new has registered, so
    // the steps have to stay up for as long as the watcher keeps asking, which is what
    // is waited for here.
    const askedSoFar = asked();
    await expect
      .poll(asked, {
        message: "the tab stopped asking the canister about its entry",
        timeout: 60_000,
      })
      .toBeGreaterThanOrEqual(askedSoFar + QUERIES_TO_OUTLAST);
    await expect(
      installPage.getByRole("heading", { name: "Set up ICP Alerts" }),
    ).toBeVisible();
    await expect(
      installPage.getByRole("heading", { name: "You're all set" }),
    ).toBeHidden();

    // A genuinely new app: its own partition, its own key, and a record the canister has
    // not heard from before. Only this may move the tab on.
    // The URL the steps put in the address bar, which is what iOS would bookmark: the
    // key the deleted app spent is one-shot, so starting over minted another.
    const replacement = await addToHomeScreen(
      browser,
      installPage.url(),
      relay.endpoint,
    );
    await expectSetUp(replacement);

    await expect(
      installPage.getByRole("heading", { name: "You're all set" }),
    ).toBeVisible({ timeout: 60_000 });

    await replacement.delete();
    await relay.close();
  });

  /**
   * A launch whose token the canister refuses, which is what every launch after the
   * first one is.
   *
   * The token is refused here for a key the canister is not waiting for rather than for
   * age, because both reach the launch as the same `token-refused`: `NoPendingLink`
   * covers a slot that was never written and one that expired alike. So this covers
   * what a refusal does to a launch. It does NOT cover the expiry arithmetic itself,
   * which has unit tests on both sides of the boundary and nothing end to end: if that
   * is what you came here for, it is not here.
   *
   * Both halves of carrying on are asserted. The screen is the visible half: a refused
   * token leaves the app on the ready screen rather than on an error. The half that
   * matters is that the launch still calls `registerForAnyAnchor`, so a push endpoint
   * the service has moved reaches the canister and delivery survives it.
   */
  test("carry on from a launch the canister refuses the token on", async ({
    browser,
    context,
    page,
    testApp,
    identities,
    signInWithIdentity,
  }) => {
    const { relay, app, installUrl } = await installOnThePhone({
      browser,
      context,
      testApp,
      identityNumber: identities[0].identityNumber,
      signInWithIdentity,
    });

    await joinTheRoomAndLeave(page);

    // A push service hands out a new endpoint whenever it likes, and only a launch
    // writes the new one, so moving it is how a launch that registered is told apart
    // from one that merely reached the right screen. Unique to this run, because the
    // chat room outlives a test and the canister keeps every subscription ever
    // registered against it: a path an earlier run used is still being posted to, and
    // would answer this for the wrong reason.
    const movedPath = `/push-moved-${Date.now()}`;
    await armRealWorkerPush(
      app.context,
      II_URL,
      `${new URL(relay.endpoint).origin}${movedPath}`,
    );

    // The launch iOS performs: the URL it saved at install, whose token no longer buys
    // anything. A refusal is not a failure to reach the canister, so it must leave the
    // app exactly as the launch before it found it.
    const relaunched = await app.context.newPage();
    await relaunched.goto(await withAKeyNothingIsWaitingFor(installUrl));
    await expect(
      relaunched.getByRole("heading", { name: "You're all set" }),
    ).toBeVisible({ timeout: 60_000 });

    // The token is dropped from the URL only where the canister took it, so a fragment
    // still there is this launch having spent one that was refused. Read after the
    // screen has settled, which is after a launch that linked would have dropped it.
    expect(
      new URL(relaunched.url()).hash,
      "the canister took this token, so no refusal was exercised",
    ).not.toBe("");

    // The half that matters, and the half a screen assertion cannot reach. The canister
    // posts to the endpoint it was last told about, so a wake-up arriving at the moved
    // one is this launch having called through to register.
    const afterMoving = relay.received();
    await expectDelivery(
      app,
      relay,
      "still registering after a refused token",
      "the app stopped delivering after a launch its token was refused on",
    );

    await expect
      .poll(() => relay.receivedPaths().slice(afterMoving), {
        message:
          "the launch never registered, so the canister still holds the endpoint from the install",
        timeout: 30_000,
      })
      .toContain(movedPath);

    await app.delete();
    await relay.close();
  });
  /**
   * The user who follows the steps, adds the app, and does not open it.
   *
   * The canister is the only party that can tell this apart from a user who added
   * nothing: it is holding a key for an install that no app has spent. Taking their word
   * instead would record the device as set up while nothing delivers, and nothing
   * anywhere would say so.
   */
  test("ask them to open an app they say they added but never opened", async ({
    context,
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

    await askToNotify(testApp.page, async (authPage: Page) => {
      await authenticate(authPage);
      const opened = authPage.context().waitForEvent("page");
      await authPage
        .getByRole("button", { name: "Allow", exact: true })
        .click();
      const installPage = await opened;
      await installPage.waitForLoadState();
      await expect(
        installPage.getByRole("heading", { name: "Set up ICP Alerts" }),
      ).toBeVisible();

      // Nothing was added to any Home Screen here: the install this tab handed out is
      // the only thing the canister is holding.
      await installPage
        .getByRole("button", { name: "I've already added it" })
        .click();
      await expect(
        installPage.getByRole("heading", { name: "Open it once to finish" }),
      ).toBeVisible({ timeout: 30_000 });

      await installPage.close();
    });

    await relay.close();
  });

  // The doorway is the page a notification opens inside the app, which then hands the
  // link to Safari. iOS resumes a standalone app on whatever page its window was left
  // on, so what matters here is that the doorway takes itself out of the window.
  test("leaves the window on the app rather than on the doorway", async ({
    browser,
  }) => {
    const phone = await browser.newContext({ userAgent: IPHONE_AGENT });
    // What iOS answers in an installed web app, as `addToHomeScreen` does: without it
    // the app's own page takes the in-browser path and leaves for `/`.
    await phone.addInitScript(() => {
      Object.defineProperty(navigator, "standalone", {
        configurable: true,
        value: true,
      });
    });
    const page = await phone.newPage();

    // Only `https:` is followed, and nothing fetches it: the hand-over is an iOS
    // scheme that no test browser answers, so the destination is never reached.
    const carried = new URLSearchParams({
      url: "https://chat.example.com/#chat",
      title: "Example App",
      body: "New message",
    });
    await page.goto(`${II_URL}/ios-icp-alerts/open#${carried.toString()}`);

    // Shown while the hand-over has not taken the page away, which it cannot here:
    // `x-safari-https:` is an iOS scheme and nothing answers it in a test browser.
    await expect(
      page.getByRole("heading", { name: "Almost there" }),
    ).toBeVisible({ timeout: 30_000 });

    // Read into the page and then dropped, so there is no fragment left for a resumed
    // window to render the offer from a second time.
    expect(new URL(page.url()).hash).toBe("");

    // A resume: the Home Screen icon and Safari's chevron are the same foreground of
    // the same window, and the icon must never come back to the offer.
    await page.evaluate(() => {
      Object.defineProperty(document, "visibilityState", {
        configurable: true,
        get: () => "hidden",
      });
      document.dispatchEvent(new Event("visibilitychange"));
      Object.defineProperty(document, "visibilityState", {
        configurable: true,
        get: () => "visible",
      });
      document.dispatchEvent(new Event("visibilitychange"));
    });

    await page.waitForURL(`${II_URL}/ios-icp-alerts`, { timeout: 30_000 });
    await expect(
      page.getByRole("heading", { name: "Almost there" }),
    ).not.toBeVisible();

    await phone.close();
  });
});
