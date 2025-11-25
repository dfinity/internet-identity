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
    await expect(
      this.#dialog.getByRole("heading", { name: "Before you continue" }),
    ).toBeHidden();
  }

  async writeDown(): Promise<string[]> {
    const heading = this.#dialog.getByRole("heading", {
      name: "Save your recovery phrase",
    });
    await expect(heading).toBeVisible();
    await this.#dialog.getByRole("button", { name: "Click to reveal" }).click();
    // We use selection and copying to the clipboard here to make sure that it
    // works correctly for power-users that don't physically write things down.
    await this.#dialog.getByRole("list").selectText();
    await this.#dialog.press("Meta+c");
    const clipboard = await readClipboard(this.#dialog.page());
    await this.#dialog
      .getByRole("button", { name: "I have written it down" })
      .click();
    await expect(heading).toBeHidden();
    return clipboard.trim().split("\n");
  }

  async verify(words: string[]): Promise<void> {
    const heading = this.#dialog.getByRole("heading", {
      name: "Verify your recovery phrase",
    });
    await expect(heading).toBeVisible();
    for (const word of words) {
      // Recovery phrase could have duplicate words, so always
      // select the first enabled button that isn't pressed.
      await this.#dialog
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

  async reset(): Promise<void> {
    const heading = this.#dialog.getByRole("heading", {
      name: "Reset your recovery phrase?",
    });
    await expect(heading).toBeVisible();
    await this.#dialog.getByRole("button", { name: "Reset" }).click();
    await expect(heading).toBeHidden();
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
    await expect(
      this.#page.getByRole("heading", {
        name: "Recovery phrase not verified",
      }),
    ).toBeVisible();

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

  test("can be activated", async ({ page }) => {
    const recoveryPhrasePage = new RecoveryPhrasePage(page);
    await recoveryPhrasePage.activate();
  });

  test.describe("can be verified", () => {
    test("when it was skipped during activation", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      const words = await recoveryPhrasePage.activate({
        skipVerification: true,
      });
      await recoveryPhrasePage.verify(words);
    });

    test("when it was skipped during reset", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      const words = await recoveryPhrasePage.reset({
        skipVerification: true,
      });
      await recoveryPhrasePage.verify(words);
    });

    test("when first attempt is in incorrect order", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      const words = await recoveryPhrasePage.activate({
        skipVerification: true,
      });

      // Swap the first word around with the next different word found,
      // compared to random shuffle, this guarantees a different phrase.
      const incorrectOrder = [...words];
      const firstWord = incorrectOrder[0];
      const differentWordIndex = incorrectOrder.findIndex(
        (word) => word !== firstWord,
      );
      incorrectOrder[0] = incorrectOrder[differentWordIndex];
      incorrectOrder[differentWordIndex] = firstWord;

      // Verify with incorrect word order
      await page.getByRole("button", { name: "Verify" }).click();
      const dialog = page.getByRole("dialog");
      const wizard = new RecoveryPhraseWizard(dialog);
      await wizard.verify(incorrectOrder);
      await expect(
        dialog.getByRole("heading", { name: "Something is wrong!" }),
      ).toBeVisible();

      // Retry, words shown should be equal to earlier words
      await dialog.getByRole("button", { name: "Retry" }).click();
      const correctWords = await wizard.writeDown();
      expect(correctWords).toEqual(words);

      // Enter correct word order
      await wizard.verify(words);
      await expect(dialog).toBeHidden();
      await expect(
        page.getByRole("heading", { name: "Recovery phrase activated" }),
      ).toBeVisible();
    });
  });

  test.describe("can be reset", () => {
    test("when it is activated", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      await recoveryPhrasePage.reset();
    });

    test("when it is unverified (skipped during activation)", async ({
      page,
    }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate({
        skipVerification: true,
      });
      await recoveryPhrasePage.reset({ isUnverified: true });
    });

    test("when it is unverified (skipped during reset)", async ({ page }) => {
      const recoveryPhrasePage = new RecoveryPhrasePage(page);
      await recoveryPhrasePage.activate();
      await recoveryPhrasePage.reset({
        skipVerification: true,
      });
      await recoveryPhrasePage.reset({ isUnverified: true });
    });
  });
});
