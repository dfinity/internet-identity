import { test as base, expect, Locator, Page } from "@playwright/test";
import { II_URL } from "../utils";

/**
 * Inject `localStorage` overrides for the email-recovery feature
 * flag before the page's JS runs. The runtime flag store at
 * `lib/state/featureFlags.ts` reads from this key on init, so this
 * is enough to flip the flag without faking the page hostname.
 *
 * The corresponding key shape is the runtime constant
 * `LOCALSTORAGE_FEATURE_FLAGS_PREFIX + name` from
 * `featureFlags.ts`. We hardcode the literal here on purpose —
 * if the runtime prefix changes we want the test to fail loudly.
 */
async function setEmailRecoveryFlag(page: Page, on: boolean) {
  await page.addInitScript(
    ({ value }) => {
      try {
        window.localStorage.setItem(
          "ii-localstorage-feature-flags__EMAIL_RECOVERY",
          JSON.stringify(value),
        );
      } catch {
        // localStorage may be locked in some test contexts.
      }
    },
    { value: on },
  );
}

class SetupEmailRecoveryWizard {
  readonly #view: Locator;

  constructor(view: Locator) {
    this.#view = view;
  }

  /** Step 1: type address and continue. */
  async enterAddress(address: string): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Add a recovery email" }),
    ).toBeVisible();
    await this.#view
      .getByRole("textbox", { name: "Email address" })
      .fill(address);
    await this.#view.getByRole("button", { name: "Continue" }).click();
  }

  /** Step 2: assert the magic-email view rendered with the right
   *  recipient and a valid-looking nonce in the Subject. */
  async expectMagicEmailView(opts: {
    recipient: "register@id.ai" | "recover@id.ai";
  }): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Send the magic email" }),
    ).toBeVisible();
    await expect(this.#view.getByText(opts.recipient)).toBeVisible();
    // The Subject token is rendered in a monospaced span; assert
    // that the canister-issued shape matches the prefix.
    await expect(
      this.#view.getByText(/II-Recovery-[0-9a-f]{16}/),
    ).toBeVisible();
  }

  async cancel(): Promise<void> {
    await this.#view.getByRole("button", { name: "Cancel" }).click();
  }
}

class EmailRecoveryFixtures {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async enableFlag(): Promise<void> {
    await setEmailRecoveryFlag(this.#page, true);
  }

  async disableFlag(): Promise<void> {
    await setEmailRecoveryFlag(this.#page, false);
  }

  async assertSetupCardVisible(): Promise<void> {
    await expect(
      this.#page.getByRole("heading", { name: "Recovery email" }),
    ).toBeVisible();
    await expect(
      this.#page.getByRole("button", { name: "Add email" }),
    ).toBeVisible();
  }

  async assertSetupCardHidden(): Promise<void> {
    // The phrase card always shows; assert specifically that the
    // *email* sub-card isn't rendered.
    await expect(
      this.#page.getByRole("heading", { name: "Recovery email" }),
    ).toBeHidden();
  }

  async assertRecoverWithEmailHidden(): Promise<void> {
    await expect(
      this.#page.getByRole("button", { name: "Recover with email" }),
    ).toBeHidden();
  }

  /**
   * Open the setup wizard from the manage-page card and run `fn`
   * against it. The wizard is closed (Cancel or Done) by `fn`.
   */
  async openSetupWizard<T>(
    fn: (wizard: SetupEmailRecoveryWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page
      .getByRole("main")
      .getByRole("button", { name: "Add email" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    const wizard = new SetupEmailRecoveryWizard(dialog);
    const value = await fn(wizard);
    return value;
  }

  /**
   * Open the recover-with-email wizard from the recovery sign-in
   * page and run `fn` against it.
   */
  async openRecoveryWizard<T>(
    fn: (wizard: SetupEmailRecoveryWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Recover with email" }).click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    // The recovery wizard reuses the same SendMagicEmail view; the
    // only differences for our purposes are the heading on step 1
    // and the recipient on step 2. The fixture shares the wizard
    // class because callers exercise the same surface.
    const wizard = new SetupEmailRecoveryWizard(dialog);
    return await fn(wizard);
  }
}

export const test = base.extend<{
  emailRecovery: EmailRecoveryFixtures;
}>({
  emailRecovery: async ({ page }, use) => {
    await use(new EmailRecoveryFixtures(page));
  },
});

export { II_URL };
