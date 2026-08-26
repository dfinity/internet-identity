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

  /** What the panel reports. */
  get state(): Locator {
    return this.page.locator("#sessionState");
  }
  /** The principal an app's canisters see. */
  get account(): Locator {
    return this.page.locator("#sessionAccountPrincipal");
  }
  /** The key II resolves the session from. */
  get sessionKey(): Locator {
    return this.page.locator("#sessionSessionPrincipal");
  }
  get sessionExpiry(): Locator {
    return this.page.locator("#sessionExpiry");
  }
  get delegation(): Locator {
    return this.page.locator("#delegationExpiry");
  }
  get replacements(): Locator {
    return this.page.locator("#delegationChanges");
  }
  /** What the siblings of a domain share. */
  get sharedHint(): Locator {
    return this.page.locator("#sessionHint");
  }
  get log(): Locator {
    return this.page.locator("#sessionLog");
  }
  get principal(): Locator {
    return this.page.locator("#principal");
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

  /** How many windows the browser holding this app has open. */
  get openWindows(): number {
    return this.page.context().pages().length;
  }

  /** Closes this tab. */
  async close(): Promise<void> {
    await this.page.close();
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
    await expect(this.state).toHaveText("signed in", { timeout: 20_000 });
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

  /** Forgets everything this origin stored, as clearing site data would. */
  async clearSiteData(): Promise<void> {
    await this.page.evaluate(async () => {
      localStorage.clear();
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
  /** The same app in another tab, window or browser. */
  openTestApp: (page: Page) => TestApp;
}>({
  testApp: async ({ page }, use) => {
    await use(new TestApp(page));
  },
  // eslint-disable-next-line no-empty-pattern
  openTestApp: async ({}, use) => {
    await use((page: Page) => new TestApp(page));
  },
});
