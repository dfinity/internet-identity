import { expect, Locator, Page, test } from "@playwright/test";
import {
  createNewIdentityInII,
  dummyAuth,
  II_URL,
  readClipboard,
} from "../utils";

class RecoveryPhraseWizard {
  #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async acknowledge(): Promise<void> {
    await expect(
      this.#dialog.getByRole("heading", { name: "Before you continue" }),
    ).toBeVisible();
    await this.#dialog.getByRole("checkbox", { name: "I acknowledge" }).check();
    await this.#dialog.getByRole("button", { name: "Continue" }).click();
  }

  async writeDown(): Promise<string[]> {
    await expect(
      this.#dialog.getByRole("heading", { name: "Save your recovery phrase" }),
    ).toBeVisible();
    await this.#dialog.getByRole("button", { name: "Click to reveal" }).click();
    await this.#dialog.getByRole("list").selectText();
    await this.#dialog.press("Meta+c");
    const clipboard = await readClipboard(this.#dialog.page());
    await this.#dialog
      .getByRole("button", { name: "I have written it down" })
      .click();
    return clipboard.trim().split("\n");
  }

  async verify(words: string[]): Promise<void> {
    await expect(
      this.#dialog.getByRole("heading", {
        name: "Verify your recovery phrase",
      }),
    ).toBeVisible();
    for (const word of words) {
      // Recovery phrase could have duplicate words, so always
      // select the first button that hasn't been pressed.
      await this.#dialog
        .getByRole("list")
        .getByRole("button", { name: word, disabled: false, pressed: false })
        .first()
        .click();
    }
  }

  async reset(): Promise<void> {
    await expect(
      this.#dialog.getByRole("heading", {
        name: "Reset your recovery phrase?",
      }),
    ).toBeVisible();
    await this.#dialog.getByRole("button", { name: "Reset" }).click();
  }
}

class RecoveryPhrasePage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async activate(options?: { skipVerification?: boolean }): Promise<string[]> {
    await expect(
      this.#page.getByRole("heading", {
        name: "Recovery phrase not activated",
      }),
    ).toBeVisible();

    await this.#page.getByRole("button", { name: "Activate" }).click();
    const dialog = this.#page.getByRole("dialog");
    const wizard = new RecoveryPhraseWizard(dialog);

    await wizard.acknowledge();
    const words = await wizard.writeDown();
    if (options?.skipVerification === true) {
      await dialog.getByRole("button", { name: "Close" }).click();
    } else {
      await wizard.verify(words);
    }

    await expect(dialog).toBeHidden();
    await expect(
      this.#page.getByRole("heading", {
        name:
          options?.skipVerification === true
            ? "Recovery phrase not verified"
            : "Recovery phrase activated",
      }),
    ).toBeVisible();

    return words;
  }

  async reset(options?: {
    isUnverified?: boolean;
    skipVerification?: boolean;
  }): Promise<string[]> {
    await expect(
      this.#page.getByRole("heading", {
        name:
          options?.isUnverified === true
            ? "Recovery phrase not verified"
            : "Recovery phrase activated",
      }),
    ).toBeVisible();

    await this.#page.getByRole("button", { name: "Reset" }).click();
    const dialog = this.#page.getByRole("dialog");
    const wizard = new RecoveryPhraseWizard(dialog);

    await wizard.reset();
    const words = await wizard.writeDown();
    if (options?.skipVerification === true) {
      await dialog.getByRole("button", { name: "Close" }).click();
    } else {
      await wizard.verify(words);
    }

    await expect(dialog).toBeHidden();
    await expect(
      this.#page.getByRole("heading", {
        name:
          options?.skipVerification === true
            ? "Recovery phrase not verified"
            : "Recovery phrase activated",
      }),
    ).toBeVisible();

    return words;
  }

  async verify(words: string[]): Promise<void> {
    await this.#page.getByRole("button", { name: "Verify" }).click();
    const dialog = this.#page.getByRole("dialog");
    const wizard = new RecoveryPhraseWizard(dialog);

    await wizard.verify(words);

    await expect(dialog).toBeHidden();
    await expect(
      this.#page.getByRole("heading", { name: "Recovery phrase activated" }),
    ).toBeVisible();
  }
}

test.describe("Recovery phrase", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(II_URL + "/manage/recovery");
    await createNewIdentityInII(page, "Test User", dummyAuth());
    await page.waitForURL(II_URL + "/manage/recovery");
  });

  test("Activate", async ({ page }) => {
    const recoveryPhrasePage = new RecoveryPhrasePage(page);
    await recoveryPhrasePage.activate();
  });

  test.describe("Verify", () => {
    test("Unverified (skipped during activation)", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      const words = await recoveryPhrasePage.activate({
        skipVerification: true,
      });
      await recoveryPhrasePage.verify(words);
    });

    test("Unverified (skipped during reset)", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      const words = await recoveryPhrasePage.reset({
        skipVerification: true,
      });
      await recoveryPhrasePage.verify(words);
    });
  });

  test.describe("Reset", () => {
    test("Activated", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      await recoveryPhrasePage.reset();
    });

    test("Unverified (skipped during activation)", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate({
        skipVerification: true,
      });
      await recoveryPhrasePage.reset({ isUnverified: true });
    });

    test("Unverified (skipped during reset)", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      await recoveryPhrasePage.reset({
        skipVerification: true,
      });
      await recoveryPhrasePage.reset({ isUnverified: true });
    });
  });
});
