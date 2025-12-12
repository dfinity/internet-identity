import { expect, Locator, Page, test as base } from "@playwright/test";
import { II_URL } from "../utils";

class RecoverIdentityWizard {
  #view: Locator;

  constructor(view: Locator) {
    this.#view = view;
  }

  async enterRecoveryPhrase(words: string[]): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Enter recovery phrase",
    });
    await expect(heading).toBeVisible();
    const clearButton = this.#view.getByRole("button", { name: "Clear all" });
    if (await clearButton.isEnabled()) {
      await clearButton.click();
    }
    for (let index = 0; index < words.length; index++) {
      await this.#view
        .getByRole("textbox", {
          name: `Word ${index + 1}`,
          exact: true,
        })
        .fill(words[index]);
    }
    await this.#view.getByRole("button", { name: "Submit" }).click();
    await expect(heading).toBeHidden();
  }

  async confirmFoundIdentity(name: string): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Identity found",
    });
    await expect(heading).toBeVisible();
    await expect(this.#view.getByText(name)).toBeVisible();
    await this.#view.getByRole("button", { name: "Continue" }).click();
    await expect(heading).toBeHidden();
  }

  async cancelFoundIdentity() {
    const heading = this.#view.getByRole("heading", {
      name: "Identity found",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Cancel" }).click();
    await expect(heading).toBeHidden();
  }

  async close(): Promise<void> {
    await this.#view.getByRole("button", { name: "Close" }).click();
  }

  async retryIdentityNotFound() {
    const heading = this.#view.getByRole("heading", {
      name: "No identity found",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Retry" }).click();
    await expect(heading).toBeHidden();
  }

  async cancelIdentityNotFound() {
    const heading = this.#view.getByRole("heading", {
      name: "No identity found",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Cancel" }).click();
    await expect(heading).toBeHidden();
  }

  async retryInvalid() {
    const heading = this.#view.getByRole("heading", {
      name: "Invalid recovery phrase",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Retry" }).click();
    await expect(heading).toBeHidden();
  }

  async cancelInvalid() {
    const heading = this.#view.getByRole("heading", {
      name: "Invalid recovery phrase",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Cancel" }).click();
    await expect(heading).toBeHidden();
  }

  async enterMissingName(newName: string): Promise<void> {
    const heading = this.#view.getByRole("heading", {
      name: "Identity found",
    });
    await expect(heading).toBeVisible();
    await this.#view
      .getByRole("textbox", { name: "Identity name" })
      .fill(newName);
    await this.#view.getByRole("button", { name: "Continue" }).click();
    await expect(heading).toBeHidden();
  }

  async cancelMissingName() {
    const heading = this.#view.getByRole("heading", {
      name: "Identity found",
    });
    await expect(heading).toBeVisible();
    await this.#view.getByRole("button", { name: "Cancel" }).click();
    await expect(heading).toBeHidden();
  }
}

class RecoveryPage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async goto() {
    await this.#page.goto(II_URL + "/recovery");
  }

  async start<T>(
    fn: (wizard: RecoverIdentityWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Get started" }).click();
    const dialog = this.#page.getByRole("dialog");
    const wizard = new RecoverIdentityWizard(dialog);
    await expect(dialog).toBeVisible();
    const value = await fn(wizard);
    await expect(dialog).toBeHidden();
    return value;
  }

  async cancel(): Promise<void> {
    await this.#page.getByRole("link", { name: "Cancel" }).click();
    await this.#page.waitForURL(II_URL + "/login");
  }
}

export const test = base.extend<{
  recoveryPage: RecoveryPage;
}>({
  recoveryPage: async ({ page }, use) => {
    await use(new RecoveryPage(page));
  },
});
