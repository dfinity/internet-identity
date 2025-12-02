import { expect, Locator, Page, test as base } from "@playwright/test";
import { II_URL, readClipboard } from "../utils";

class CreateRecoveryPhraseWizard {
  #view: Locator;

  constructor(view: Locator) {
    this.#view = view;
  }

  async acknowledge(): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Before you continue" }),
    ).toBeVisible();
    await this.#view.getByRole("checkbox", { name: "I acknowledge" }).check();
    await this.#view.getByRole("button", { name: "Continue" }).click();
    await expect(
      this.#view.getByRole("heading", { name: "Before you continue" }),
    ).toBeHidden();
  }

  async writeDown(): Promise<string[]> {
    const heading = this.#view.getByRole("heading", {
      name: "Save your recovery phrase",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Click to reveal" }).click();
    // We use selection and copying to the clipboard here to make sure that it
    // works correctly for power-users that don't physically write things down.
    await this.#view.getByRole("list").selectText();
    await this.#view
      .page()
      .keyboard.press(process.platform === "darwin" ? "Meta+C" : "Control+C");
    const clipboard = await readClipboard(this.#view.page());
    await this.#view
      .getByRole("button", { name: "I have written it down" })
      .click();
    await expect(heading).toBeHidden();
    return clipboard.trim().split("\n");
  }

  async verifySelecting(words: string[]): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Verify your recovery phrase",
    });
    await expect(heading).toBeVisible();
    for (const word of words) {
      // Recovery phrase could have duplicate words, so always
      // select the first enabled button that isn't pressed.
      await this.#view
        .getByRole("list")
        .getByRole("button", {
          name: word,
          exact: true,
          disabled: false,
          pressed: false,
        })
        .first()
        .click();
    }
    await expect(heading).toBeHidden();
  }

  async verifyTyping(words: string[]): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Verify your recovery phrase",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Clear all" }).click();
    for (let index = 0; index < words.length; index++) {
      await this.#view
        .getByRole("textbox", {
          name: `Word ${index + 1}`,
          exact: true,
        })
        .fill(words[index]);
    }
    const submitButton = this.#view.getByRole("button", { name: "Submit" });
    if (await submitButton.isVisible()) {
      await submitButton.click();
    }
    await expect(heading).toBeHidden();
  }

  async confirmReset(): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Reset your recovery phrase?",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Reset" }).click();
    await expect(heading).toBeHidden();
  }

  async cancelReset(): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Reset your recovery phrase?",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Cancel" }).click();
    await expect(heading).toBeHidden();
  }

  async close(): Promise<void> {
    await this.#view.getByRole("button", { name: "Close" }).click();
  }

  async retry(): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Something is wrong!" }),
    ).toBeVisible();
    await this.#view.getByRole("button", { name: "Retry" }).click();
    await expect(
      this.#view.getByRole("heading", { name: "Something is wrong!" }),
    ).toBeHidden();
  }
}

class ManageRecoveryPage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async goto() {
    await this.#page.goto(II_URL + "/manage/recovery");
  }

  async activate<T>(
    fn: (wizard: CreateRecoveryPhraseWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Activate" }).click();
    return this.#withWizard(fn);
  }

  async reset<T>(
    fn: (wizard: CreateRecoveryPhraseWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Reset" }).click();
    return this.#withWizard(fn);
  }

  async verify<T>(
    fn: (wizard: CreateRecoveryPhraseWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Verify" }).click();
    return this.#withWizard(fn);
  }

  async assertNotActivated() {
    await expect(
      this.#page.getByRole("heading", {
        name: "Recovery phrase not activated",
      }),
    ).toBeVisible();
  }

  async assertNotVerified() {
    await expect(
      this.#page.getByRole("heading", { name: "Recovery phrase not verified" }),
    ).toBeVisible();
  }

  async assertActivated() {
    await expect(
      this.#page.getByRole("heading", { name: "Recovery phrase activated" }),
    ).toBeVisible();
  }

  async #withWizard<T>(
    fn: (wizard: CreateRecoveryPhraseWizard) => Promise<T>,
  ): Promise<T> {
    const dialog = this.#page.getByRole("dialog");
    const wizard = new CreateRecoveryPhraseWizard(dialog);
    await expect(dialog).toBeVisible();
    const value = await fn(wizard);
    await expect(dialog).toBeHidden();
    return value;
  }
}

export const test = base.extend<{
  manageRecoveryPage: ManageRecoveryPage;
}>({
  manageRecoveryPage: async ({ page }, use) => {
    await use(new ManageRecoveryPage(page));
  },
});
