import {
  expect,
  type Browser,
  type BrowserContext,
  type Page,
} from "@playwright/test";
import { armRealWorkerPush, shownByTheWorker } from "./notifications";
import { II_URL } from "../utils";

/**
 * The iPhone, and the Home Screen app on it.
 *
 * Two things a browser cannot be talked into have to be arranged here. The platform,
 * because what Internet Identity offers is decided from the user agent and iOS is where
 * web push lives in an installed app rather than in the browser. And the app itself,
 * which iOS runs in a storage partition of its own: that is a separate browser context,
 * which is exactly what separate storage means, and a document that answers
 * `navigator.standalone`, which is what the page asks.
 *
 * Everything else is real: the same route, the same service worker, the same canister.
 */
export const IPHONE_AGENT =
  "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1";

export interface HomeScreenApp {
  context: BrowserContext;
  page: Page;
  /** Closes the app the way deleting it from the Home Screen does: the partition goes,
   *  and the key in it with it. Nothing tells the canister. */
  delete: () => Promise<void>;
}

/**
 * Adds the app to the Home Screen and opens it, which on a real phone is the user
 * following the steps. The URL is the one the install page was showing, because that is
 * what iOS bookmarks.
 */
export const addToHomeScreen = async (
  browser: Browser,
  url: string,
  relayEndpoint: string,
): Promise<HomeScreenApp> => {
  const context = await browser.newContext({
    userAgent: IPHONE_AGENT,
    viewport: { width: 393, height: 852 },
    isMobile: true,
    hasTouch: true,
  });
  // What iOS answers in an installed web app, and the first thing the page asks.
  await context.addInitScript(() => {
    Object.defineProperty(navigator, "standalone", {
      configurable: true,
      value: true,
    });
  });
  await armRealWorkerPush(context, II_URL, relayEndpoint);

  const page = await context.newPage();
  await page.goto(url);
  return {
    context,
    page,
    delete: () => context.close(),
  };
};

/** Waits until the app says it is set up, which is the screen the user is left on. */
export const expectSetUp = async (app: HomeScreenApp): Promise<void> => {
  await expect(
    app.page.getByRole("heading", { name: "You're all set" }),
  ).toBeVisible({ timeout: 30_000 });
};

/**
 * Asks the app to request notification consent, and says what happened if the ceremony
 * does not finish.
 *
 * Answering takes a while here: it opens the install page and reads it. So the wait for
 * the window to close is armed before answering rather than after, and a window already
 * gone counts as closed. Where it really does stall, the window's console and what it
 * was showing go to the test output, because every way this flow can stall looks the
 * same from outside.
 */
export const askToNotify = async (
  appPage: Page,
  answer: (authPage: Page) => Promise<void>,
): Promise<void> => {
  const opened = appPage.context().waitForEvent("page");
  await appPage.getByRole("button", { name: "Ask to notify" }).click();
  const authPage = await opened;
  authPage.on("console", (message) =>
    console.log(`[authorize console] ${message.type()}: ${message.text()}`),
  );
  authPage.on("pageerror", (error) =>
    console.log(`[authorize pageerror] ${error.message}`),
  );
  // Armed before the answer runs, never after: answering here opens the install page
  // and reads it, which takes long enough that the ceremony can finish first, and
  // `waitForEvent` on a window that has already closed waits for an event that will
  // never come again.
  const closed = authPage
    .waitForEvent("close", { timeout: 60_000 })
    .then(() => true)
    .catch(() => false);
  await answer(authPage);

  try {
    if (!(await closed) && !authPage.isClosed()) {
      throw new Error("the authorize window never closed");
    }
  } catch (error) {
    const showing = authPage.isClosed()
      ? "(closed)"
      : await authPage.locator("body").innerText();
    console.log(
      `[authorize url] ${authPage.isClosed() ? "(closed)" : authPage.url()}`,
    );
    console.log(
      `[authorize screen] ${showing.replace(/\s+/g, " ").slice(0, 600)}`,
    );
    throw error;
  }
};

/**
 * What the app has on screen, waited for and reported. Asserts nothing.
 *
 * A wake-up that shows nothing at all is a different failure from one that shows the
 * placeholder: the first means the worker never ran, the second means it ran and could
 * not read the app's text. Only one of them is about this feature, so the scenarios say
 * which they got.
 */
export const reportShown = async (
  app: HomeScreenApp,
  when: string,
): Promise<{ title: string; body: string }[]> => {
  let shown: { title: string; body: string }[] = [];
  try {
    await expect
      .poll(
        async () => {
          shown = await shownByTheWorker(app.context);
          return shown.length;
        },
        { message: `the app showed nothing ${when}`, timeout: 30_000 },
      )
      .toBeGreaterThan(0);
  } catch {
    // Swallowed on purpose: an empty screen is one of the two answers this reports, and
    // which of them is a failure is the scenario's to say.
  }
  console.log(
    `[app ${when}] workers=${app.context.serviceWorkers().length} shown=${JSON.stringify(shown)}`,
  );
  return shown;
};
