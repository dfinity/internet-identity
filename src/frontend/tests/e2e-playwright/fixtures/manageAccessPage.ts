import { expect, Locator, Page, test as base } from "@playwright/test";
import { DummyAuthFn, II_URL } from "../utils";

export const DEFAULT_PASSKEY_NAME = "Chrome";

class Passkey {
  #item: Locator;

  constructor(item: Locator) {
    this.#item = item;
  }

  async rename(name: string): Promise<void> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await this.#item
      .getByRole("menu")
      .getByRole("menuitem", { name: "Rename" })
      .click();
    const dialog = this.#item.page().getByRole("dialog");
    await dialog.getByRole("textbox", { name: "Passkey name" }).fill(name);
    await dialog.getByRole("button", { name: "Save changes" }).click();
  }

  async remove(isInUse: boolean): Promise<void> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await this.#item
      .getByRole("menu")
      .getByRole("menuitem", { name: "Remove" })
      .click();
    const dialog = this.#item.page().getByRole("dialog");
    const warningText = "currently signed in";
    if (isInUse) {
      await expect(dialog).toHaveText(warningText);
    } else {
      await expect(dialog).not.toHaveText(warningText);
    }
    await dialog.getByRole("button", { name: "Remove passkey" }).click();
  }

  async assertUnremovable(): Promise<void> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await expect(
      this.#item.getByRole("menu").getByRole("menuitem", { name: "Remove" }),
    ).toBeHidden();
  }
}

class AddWizard {
  #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async passkey(auth: DummyAuthFn): Promise<void> {
    await this.#dialog
      .getByRole("button", { name: "Continue with passkey" })
      .click();
    await expect(
      this.#dialog.getByRole("heading", {
        name: /Add a passkey|Add another passkey/,
      }),
    ).toBeVisible();
    auth(this.#dialog.page());
    await this.#dialog.getByRole("button", { name: "Create passkey" }).click();
  }

  async assertPasskeyUnavailable(): Promise<void> {
    await expect(
      this.#dialog.getByRole("button", { name: "Continue with passkey" }),
    ).toBeDisabled();
  }

  async close(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Close" }).click();
  }
}

class ManageAccessPage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async goto() {
    await this.#page.goto(II_URL + "/manage/access");
  }

  async add<T>(fn: (wizard: AddWizard) => Promise<T>): Promise<T> {
    await this.#page
      .getByRole("main")
      .getByRole("button", { name: "Add new" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    const wizard = new AddWizard(dialog);
    const value = await fn(wizard);
    await expect(dialog).toBeHidden();
    return value;
  }

  findPasskey(name: string): Passkey {
    const item = this.#page
      .getByRole("main")
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: name })
      .first();
    return new Passkey(item);
  }

  async assertPasskeyExists(name: string): Promise<void> {
    await expect(
      this.#page
        .getByRole("main")
        .getByRole("listitem")
        .filter({ hasText: "Passkey" })
        .filter({ hasText: name }),
    ).toBeVisible();
  }

  async assertPasskeyCount(count: number): Promise<void> {
    await expect(
      this.#page
        .getByRole("main")
        .getByRole("listitem")
        .filter({ hasText: "Passkey" }),
    ).toHaveCount(count);
  }
}

export const test = base.extend<{
  manageAccessPage: ManageAccessPage;
}>({
  manageAccessPage: async ({ page }, use) => {
    await use(new ManageAccessPage(page));
  },
});
