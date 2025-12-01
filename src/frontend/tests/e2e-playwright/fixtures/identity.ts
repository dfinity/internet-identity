import { Page, test as base } from "@playwright/test";
import { dummyAuth, DummyAuthFn, II_URL } from "../utils";

interface Identity {
  name: string;
  signIn(): Promise<void>;
  signOut(): Promise<void>;
}

const DEFAULT_NAME = "Test User";

class IdentityWizard {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signInWithPasskey(auth: DummyAuthFn): Promise<void> {
    await this.#goto();
    await this.#page
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    auth(this.#page);
    await this.#page
      .getByRole("button", { name: "Use existing identity" })
      .click();
  }

  async signUpWithPasskey(auth: DummyAuthFn, name: string): Promise<void> {
    await this.#goto();
    await this.#page
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    await this.#page
      .getByRole("button", { name: "Create new identity" })
      .click();
    await this.#page.getByLabel("Identity name").fill(name);
    auth(this.#page);
    await this.#page.getByRole("button", { name: "Create identity" }).click();
  }

  /**
   * Used to navigate to identity sign in/up view, attempts to click a sequence
   * of buttons in order (if visible) to get to the intended view from any page.
   */
  async #goto(): Promise<void> {
    const buttons = [
      this.#page.getByRole("button", { name: "Manage identity" }),
      this.#page.getByRole("button", { name: "Switch identity" }),
      this.#page.getByRole("button", { name: "Use another identity" }),
    ];
    // Wait for any of these buttons to appear
    await Promise.race(buttons.map((button) => button.waitFor()));
    // Click the buttons in order (if visible).
    for (const button of buttons) {
      if (await button.isVisible()) {
        await button.click();
      }
    }
  }
}

export const test = base.extend<{
  identity: Identity;
}>({
  identity: async ({ page, browser }, use) => {
    const auth = dummyAuth();
    const temptContext = await browser.newContext();
    const tempPage = await temptContext.newPage();
    await tempPage.goto(II_URL + "/login");
    const tempWizard = new IdentityWizard(tempPage);
    await tempWizard.signUpWithPasskey(auth, DEFAULT_NAME);
    await tempPage.waitForURL(II_URL + "/manage");
    await tempPage.close();
    await use({
      name: DEFAULT_NAME,
      signIn: async (): Promise<void> => {
        const wizard = new IdentityWizard(page);
        await wizard.signInWithPasskey(auth);
      },
      signOut: async (): Promise<void> => {
        // Navigating to landing page (reloading) is sufficient for now to sign out
        await page.goto(II_URL);
      },
    });
  },
});
