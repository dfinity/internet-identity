import {
  expect,
  test as base,
  type Locator,
  type Page,
} from "@playwright/test";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";
import { II_URL, TEST_APP_URL } from "../utils";

/**
 * The test app, addressed by what it does rather than by which element does it.
 *
 * The session panel is the reason this exists: a scenario about sessions is
 * mostly assertions on what the panel says, and spelling those as `#sessionState`
 * in every test buries the scenario in selectors.
 */

const II_CANISTER_ID = readCanisterId({ canisterName: "internet_identity" });

/** What the panel prints, so no scenario has to quote it. */
const HOLDS_SESSION = "signed in";
const HOLDS_NOTHING = "no session";
const NO_ACCOUNT = "-";
const NO_DELEGATION = "none held";
const NOTHING_SHARED = "none";

/**
 * How long a scenario waits on something the app has to reach a canister for: a
 * mint, a re-issue, or discovering that its session has been revoked.
 */
const ROUND_TRIP = 30_000;

export interface TestAppOptions {
  /** Defaults to `TEST_APP_URL`. */
  url?: string;
  /** The identity provider's authorize URL. Defaults to the local II. */
  authorizeUrl?: string;
  /**
   * Which protocol the sign-in button uses. `"session"` is `AuthClient` and
   * creates a session; `"legacy"` is the raw postMessage flow and creates none.
   * Always stated, so the app's own default never decides what a test exercises.
   */
  protocol?: "session" | "legacy";
  derivationOrigin?: string;
  /** Announces the session across the siblings of this domain. */
  cookieDomain?: string;
}

export class TestApp {
  readonly page: Page;

  constructor(page: Page) {
    this.page = page;
  }

  private get state(): Locator {
    return this.page.locator("#sessionState");
  }
  private get account(): Locator {
    return this.page.locator("#sessionAccountPrincipal");
  }
  private get sessionKey(): Locator {
    return this.page.locator("#sessionSessionPrincipal");
  }
  private get delegation(): Locator {
    return this.page.locator("#delegationExpiry");
  }
  private get replacements(): Locator {
    return this.page.locator("#delegationChanges");
  }
  private get sharedHint(): Locator {
    return this.page.locator("#sessionHint");
  }
  private get log(): Locator {
    return this.page.locator("#sessionLog");
  }

  /** Opens the app and states which provider and protocol to use. */
  async open(options: TestAppOptions = {}): Promise<void> {
    await this.page.goto(options.url ?? TEST_APP_URL);
    await this.page
      .getByRole("textbox", { name: "Identity Provider" })
      .fill(options.authorizeUrl ?? `${II_URL}/authorize`);
    await this.page
      .getByRole("checkbox", { name: "Use ICRC-25 and sessions:" })
      .setChecked((options.protocol ?? "session") === "session");
    if ((options.protocol ?? "session") === "session") {
      // The client refuses a session chain naming any provider but the one it was
      // configured with, and its default is the mainnet canister.
      await this.page
        .getByRole("textbox", { name: "II canister id:" })
        .fill(II_CANISTER_ID);
    }
    if (options.derivationOrigin !== undefined) {
      await this.page
        .locator("#derivationOrigin")
        .fill(options.derivationOrigin);
    }
    if (options.cookieDomain !== undefined) {
      await this.page.locator("#sessionStorageCookie").check();
      await this.page
        .locator("#sessionCookieDomain")
        .fill(options.cookieDomain);
      await this.page.locator("#sessionCookieDomain").blur();
    }
  }

  /** Closes this tab. */
  async close(): Promise<void> {
    await this.page.close();
  }

  /** Waits until the app holds a session. */
  async waitUntilSignedIn(): Promise<void> {
    await expect(this.state).toHaveText(HOLDS_SESSION, { timeout: ROUND_TRIP });
  }

  /** Fails unless the app is holding nothing it could act with. */
  async expectSignedOut(): Promise<void> {
    await expect(this.state).toHaveText(HOLDS_NOTHING, { timeout: ROUND_TRIP });
  }

  /** Fails unless the app is acting as some account. */
  async expectHoldsAccount(): Promise<void> {
    await expect(this.account).not.toHaveText(NO_ACCOUNT);
  }

  /** The principal an app's canisters see, once the app has one. */
  async accountPrincipal(): Promise<string> {
    await this.expectHoldsAccount();
    return this.account.innerText();
  }

  async expectAccount(principal: string): Promise<void> {
    await expect(this.account).toHaveText(principal);
  }

  async expectAccountOtherThan(principal: string): Promise<void> {
    await expect(this.account).not.toHaveText(principal);
    await expect(this.account).not.toHaveText(NO_ACCOUNT);
  }

  /** The key II resolves the session from, once the app has one. */
  async sessionKeyPrincipal(): Promise<string> {
    await expect(this.sessionKey).not.toHaveText(NO_ACCOUNT);
    return this.sessionKey.innerText();
  }

  async expectSessionKeyOtherThan(principal: string): Promise<void> {
    await expect(this.sessionKey).not.toHaveText(principal);
  }

  /** Fails unless the app has a delegation it could sign a request with. */
  async expectHoldsDelegation(): Promise<void> {
    await expect(this.delegation).not.toHaveText(NO_DELEGATION);
  }

  async expectNoDelegationReplacements(): Promise<void> {
    await expect(this.replacements).toHaveText("0");
  }

  async expectDelegationReplaced(): Promise<void> {
    await expect(this.replacements).not.toHaveText("0", {
      timeout: ROUND_TRIP,
    });
  }

  /** Fails unless the domain is announcing a session to its subdomains. */
  async expectSharesSession(): Promise<void> {
    await expect(this.sharedHint).not.toHaveText(NOTHING_SHARED);
    // What crosses between siblings names the account and an expiry.
    await expect(this.sharedHint).toContainText("until");
  }

  async expectSharesNothing(): Promise<void> {
    await expect(this.sharedHint).toHaveText(NOTHING_SHARED, {
      timeout: ROUND_TRIP,
    });
  }

  async expectSilentReauthSucceeded(): Promise<void> {
    await expect(this.log).toContainText("silent re-auth", {
      timeout: ROUND_TRIP,
    });
    await this.waitUntilSignedIn();
  }

  async expectSilentReauthFailed(): Promise<void> {
    await expect(this.log).toContainText("error", { timeout: ROUND_TRIP });
    await this.expectSignedOut();
  }

  /**
   * Runs something and fails if the browser opened a window while it ran.
   *
   * Counting the windows afterwards would miss one that opened and closed
   * again, which is the thing a scenario about rendering nothing cares about.
   */
  async expectNothingOpens(action: () => Promise<void>): Promise<void> {
    const opened: Page[] = [];
    const record = (page: Page): number => opened.push(page);
    const context = this.page.context();
    context.on("page", record);
    try {
      await action();
    } finally {
      context.off("page", record);
    }
    expect(opened).toHaveLength(0);
  }

  /** Arrives at the app without filling anything in, as a second tab does. */
  async visit(url: string = TEST_APP_URL): Promise<void> {
    await this.page.goto(url);
  }

  /** Loads the app again in the same tab. */
  async reload(): Promise<void> {
    await this.page.reload();
  }

  /** Brings this tab forward without announcing it, which a click would. */
  async focus(): Promise<void> {
    await this.page.bringToFront();
  }

  /**
   * Runs a sign-in and waits for the app to actually hold a session.
   *
   * The provider's window closes when it is done, which is before the app has
   * minted and stored anything, so waiting on the window alone races the result.
   */
  async signIn(authenticate: (authPage: Page) => Promise<void>): Promise<void> {
    const authPagePromise = this.page.context().waitForEvent("page");
    await this.page.getByRole("button", { name: "Sign In" }).click();
    const authPage = await authPagePromise;
    await authenticate(authPage);
    await authPage.waitForEvent("close", { timeout: 15_000 });
    await this.waitUntilSignedIn();
  }

  /** Signs in and abandons the ceremony part way through. */
  async abandonSignIn(part: (authPage: Page) => Promise<void>): Promise<void> {
    const authPagePromise = this.page.context().waitForEvent("page");
    await this.page.getByRole("button", { name: "Sign In" }).click();
    const authPage = await authPagePromise;
    await part(authPage);
    await authPage.close();
  }

  async signOut(): Promise<void> {
    await this.page.getByRole("button", { name: "Sign out" }).first().click();
  }

  /** Asks for a replacement now rather than waiting for one to be due. */
  async replaceDelegation(): Promise<void> {
    await this.page.getByRole("button", { name: "Refresh now" }).click();
  }

  /** Asks the provider to re-issue without rendering anything. */
  async silentReauth(): Promise<void> {
    await this.page.getByRole("button", { name: "Silent re-auth" }).click();
  }

  /** Makes a real canister call as whoever the app is acting as. */
  async whoAmI(): Promise<void> {
    await this.page.getByRole("button", { name: "Who am I?" }).click();
    await expect(this.page.locator("#whoamiResponse")).not.toHaveText(
      "Loading...",
      { timeout: 30_000 },
    );
  }

  /** Brings this tab forward, which is a trigger for replacing a delegation. */
  async returnToTab(): Promise<void> {
    await this.page.bringToFront();
    await this.page.evaluate(() =>
      document.dispatchEvent(new Event("visibilitychange")),
    );
  }

  /** Moves this page past the point where its delegation is due. */
  async ageDelegation(duration = "05:30"): Promise<void> {
    await this.page.clock.install();
    await this.page.clock.fastForward(duration);
  }

  /**
   * Forgets everything this origin stored, as clearing site data would.
   *
   * Cookies included: a domain announces a session to its subdomains in one,
   * so a clean start that left them would not be one.
   */
  async clearSiteData(): Promise<void> {
    await this.page.context().clearCookies();
    await this.page.evaluate(async () => {
      localStorage.clear();
      sessionStorage.clear();
      const databases = (await indexedDB.databases?.()) ?? [];
      await Promise.all(
        databases.map(
          ({ name }) =>
            new Promise((resolve) => {
              if (name === undefined) return resolve(undefined);
              const request = indexedDB.deleteDatabase(name);
              request.onsuccess =
                request.onerror =
                request.onblocked =
                  () => resolve(undefined);
            }),
        ),
      );
    });
  }

  /**
   * Lists origins as alternatives of the derivation origin they share.
   *
   * One canister serves every host the dev server maps here, so this is written
   * once and every one of them reads the same list.
   */
  async declareAlternativeOrigins(origins: string[]): Promise<void> {
    const alternativeOrigins = JSON.stringify({ alternativeOrigins: origins });
    await this.page.locator("#hostUrl").fill("https://localhost:5173");
    await this.page.locator("#newAlternativeOrigins").fill(alternativeOrigins);
    await this.page.locator("#certified").click();
    await this.page.locator("#updateNewAlternativeOrigins").click();
    await expect(this.page.locator("#alternativeOrigins")).toHaveText(
      alternativeOrigins,
      { timeout: 15_000 },
    );
  }
}

export const test = base.extend<{
  testApp: TestApp;
  /**
   * The app once it holds a session, for a hook that reads what a sign-in left
   * behind.
   *
   * `afterEach` runs before the sign-in fixture's own teardown, so the
   * provider's window may still be closing and the app may still be minting.
   * Waiting here rather than in each hook keeps that out of the scenarios, the
   * way `authorizedPrincipal` does for the specs that read a principal.
   *
   * Only a hook should ask for this. A test body that asks for it waits for a
   * sign-in that has not been started yet.
   */
  signedInApp: TestApp;
  /** The same app in another tab, window or browser. */
  openTestApp: (page: Page) => TestApp;
}>({
  testApp: async ({ page }, use) => {
    await use(new TestApp(page));
  },
  signedInApp: async ({ page }, use) => {
    const app = new TestApp(page);
    await app.waitUntilSignedIn();
    await use(app);
  },
  // eslint-disable-next-line no-empty-pattern
  openTestApp: async ({}, use) => {
    await use((page: Page) => new TestApp(page));
  },
});
