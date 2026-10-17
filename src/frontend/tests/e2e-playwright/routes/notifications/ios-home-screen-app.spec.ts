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
  // Sign-in settles when the user leaves the handoff, not when they answer: the screen
  // stays up carrying the link, for a browser that opened no tab.
  await authPage.getByRole("button", { name: "Continue", exact: true }).click();
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
  body: string,
  message: string,
): Promise<void> => {
  await expect
    .poll(
      async () => {
        const shown = (await shownByTheWorker(app.context)).map(
          (one) => one.body,
        );
        if (!shown.includes(body)) {
          await deliverPush(app.page, II_URL);
        }
        return shown;
      },
      { message, timeout: 60_000 },
    )
    .toContain(body);
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

  await expectTheAppToShow(app, text, message);
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
    await notifications.click();
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

  test("keep the install steps up until a new app delivers", async ({
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

    // Deleted from the Home Screen, and the push service never told. The record it
    // leaves behind still says subscribed and enabled, and goes on saying so: nothing
    // bounces, because the relay answers for that endpoint exactly as before.
    await app.delete();

    // Back in the browser, which reads that record and believes it.
    const installPage = await context.newPage();
    const asked = queriesSent(installPage);
    await installPage.goto(installUrl);
    await expect(
      installPage.getByRole("heading", { name: "You're all set" }),
    ).toBeVisible({ timeout: 60_000 });

    // The user is the only party who can see their own Home Screen, and what they say
    // is the only thing that moves this on.
    await installPage
      .getByRole("button", { name: "I don't have the app any more" })
      .click();
    await expect(
      installPage.getByRole("heading", { name: "Add ICP Alerts again" }),
    ).toBeVisible({ timeout: 30_000 });

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
      installPage.getByRole("heading", { name: "Add ICP Alerts again" }),
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
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });

    await relay.close();
  });
});
