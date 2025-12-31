import { expect, Locator, Page, test as base } from "@playwright/test";
import { DummyAuthFn, II_URL } from "../utils";

export const DEFAULT_PASSKEY_NAME = "Chrome";

class RenamePasskeyDialog {
  #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async fill(value: string): Promise<void> {
    await this.#dialog
      .getByRole("textbox", { name: "Passkey name" })
      .fill(value);
  }

  async submit(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Save changes" }).click();
  }

  async assertSubmitDisabled(): Promise<void> {
    await expect(
      this.#dialog.getByRole("button", { name: "Save changes" }),
    ).toBeDisabled();
  }

  async close(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Close" }).click();
  }
}

class RemovePasskeyDialog {
  #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async confirm(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Remove passkey" }).click();
  }

  async cancel(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Cancel" }).click();
  }

  async assertSignOutWarningShown(): Promise<void> {
    await expect(this.#dialog).toHaveText(/you will be signed out/);
  }
}

class PasskeyItem {
  #item: Locator;

  constructor(item: Locator) {
    this.#item = item;
  }

  async rename<T>(fn: (dialog: RenamePasskeyDialog) => Promise<T>): Promise<T> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await this.#item
      .getByRole("menu")
      .getByRole("menuitem", { name: "Rename" })
      .click();
    const dialog = this.#item.page().getByRole("dialog").filter({
      hasText: "Rename passkey",
    });
    await expect(dialog).toBeVisible();
    const renamePasskeyDialog = new RenamePasskeyDialog(dialog);
    const value = await fn(renamePasskeyDialog);
    await expect(dialog).toBeHidden();
    return value;
  }

  async remove<T>(fn: (dialog: RemovePasskeyDialog) => Promise<T>): Promise<T> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await this.#item
      .getByRole("menu")
      .getByRole("menuitem", { name: "Remove" })
      .click();
    const dialog = this.#item.page().getByRole("dialog").filter({
      hasText: "Are you sure?",
    });
    await expect(dialog).toBeVisible();
    const removePasskeyDialog = new RemovePasskeyDialog(dialog);
    const value = await fn(removePasskeyDialog);
    await expect(dialog).toBeHidden();
    return value;
  }

  async assertUnremovable(): Promise<void> {
    await this.#item.getByRole("button", { name: "More options" }).click();
    await expect(
      this.#item.getByRole("menu").getByRole("menuitem", { name: "Remove" }),
    ).toBeHidden();
  }
}

class AddDialog {
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

  async add<T>(fn: (dialog: AddDialog) => Promise<T>): Promise<T> {
    await this.#page
      .getByRole("main")
      .getByRole("button", { name: "Add new" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    const addDialog = new AddDialog(dialog);
    const value = await fn(addDialog);
    await expect(dialog).toBeHidden();
    return value;
  }

  findPasskey(name: string): PasskeyItem {
    const item = this.#page
      .getByRole("main")
      .getByRole("listitem")
      .filter({ hasText: "Passkey" })
      .filter({ hasText: name })
      .first();
    return new PasskeyItem(item);
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
