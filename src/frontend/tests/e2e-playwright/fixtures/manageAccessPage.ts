import { expect, Locator, Page, test as base } from "@playwright/test";
import { DummyAuthFn, II_URL } from "../utils";

export const DEFAULT_PASSKEY_NAME = "Chrome";

class RenamePasskeyDialog {
  readonly #dialog: Locator;
  readonly #onChange: (value: string) => void;

  constructor(dialog: Locator, onChange: (value: string) => void) {
    this.#dialog = dialog;
    this.#onChange = onChange;
  }

  get locator(): Locator {
    return this.#dialog.filter({
      has: this.#dialog.getByRole("heading", { name: "Rename passkey" }),
    });
  }

  async fill(value: string): Promise<void> {
    await this.locator
      .getByRole("textbox", { name: "Passkey name" })
      .fill(value);
  }

  async submit(): Promise<void> {
    await this.locator.getByRole("button", { name: "Save changes" }).click();
    const value = await this.locator
      .getByRole("textbox", { name: "Passkey name" })
      .inputValue();
    this.#onChange(value);
  }

  async assertSubmitDisabled(): Promise<void> {
    await expect(
      this.locator.getByRole("button", { name: "Save changes" }),
    ).toBeDisabled();
  }

  async close(): Promise<void> {
    await this.locator.getByRole("button", { name: "Close" }).click();
  }
}

class RemovePasskeyDialog {
  readonly #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  get locator(): Locator {
    return this.#dialog.filter({
      has: this.#dialog.getByRole("heading", { name: "Are you sure?" }),
    });
  }

  async confirm(): Promise<void> {
    await this.locator.getByRole("button", { name: "Remove passkey" }).click();
  }

  async cancel(): Promise<void> {
    await this.locator.getByRole("button", { name: "Cancel" }).click();
  }

  async assertSignOutWarningShown(): Promise<void> {
    await expect(this.locator).toHaveText(/you will be signed out/);
  }
}

class PasskeyItem {
  readonly #item: Locator;
  #name: string;

  constructor(item: Locator, name: string) {
    this.#item = item;
    this.#name = name;
  }

  get locator(): Locator {
    return this.#item.filter({ hasText: this.#name }).first();
  }

  async rename<T>(fn: (dialog: RenamePasskeyDialog) => Promise<T>): Promise<T> {
    await this.locator.getByRole("button", { name: "More options" }).click();
    await this.locator
      .getByRole("menu")
      .getByRole("menuitem", { name: "Rename" })
      .click();
    const dialog = new RenamePasskeyDialog(
      this.locator.page().getByRole("dialog"),
      (value) => (this.#name = value),
    );
    await expect(dialog.locator).toBeVisible();
    const value = await fn(dialog);
    await expect(dialog.locator).toBeHidden();
    return value;
  }

  async remove<T>(fn: (dialog: RemovePasskeyDialog) => Promise<T>): Promise<T> {
    await this.locator.getByRole("button", { name: "More options" }).click();
    await this.locator
      .getByRole("menu")
      .getByRole("menuitem", { name: "Remove" })
      .click();
    const dialog = new RemovePasskeyDialog(
      this.locator.page().getByRole("dialog"),
    );
    await expect(dialog.locator).toBeVisible();
    const value = await fn(dialog);
    await expect(dialog.locator).toBeHidden();
    return value;
  }

  async assertUnremovable(): Promise<void> {
    await this.locator.getByRole("button", { name: "More options" }).click();
    await expect(
      this.locator.getByRole("menu").getByRole("menuitem", { name: "Remove" }),
    ).toBeHidden();
  }
}

class AddDialog {
  readonly #dialog: Locator;

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
  readonly #page: Page;

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
      .first();
    return new PasskeyItem(item, name);
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
