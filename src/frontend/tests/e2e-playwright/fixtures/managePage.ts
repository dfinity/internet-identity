import { Page, test as base, expect, Locator } from "@playwright/test";
import { II_URL } from "../utils";

class RemoveIdentityConfirmation {
  readonly #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async confirm(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Remove" }).click();
  }

  async cancel(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Cancel" }).click();
  }
}

class SignOutConfirmation {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async keepIdentity(): Promise<void> {
    await this.#page
      .getByRole("button", { name: "Sign out and keep identity" })
      .click();
    await this.#page.waitForURL(II_URL);
  }

  async removeFromDevice(): Promise<void> {
    await this.#page
      .getByRole("button", { name: "Sign out and remove from device" })
      .click();
    await this.#page.waitForURL(II_URL);
  }
}

class ManageIdentitiesDialog {
  readonly #dialog: Locator;

  constructor(dialog: Locator) {
    this.#dialog = dialog;
  }

  async remove<T>(
    name: string,
    fn: (confirmation: RemoveIdentityConfirmation) => Promise<T>,
  ): Promise<T> {
    const row = this.#dialog.locator("div").filter({ hasText: name });
    await row.getByRole("button", { name: "Remove" }).click();
    await expect(
      this.#dialog.getByRole("heading", { name: "Remove from this device" }),
    ).toBeVisible();
    const confirmation = new RemoveIdentityConfirmation(this.#dialog);
    return fn(confirmation);
  }

  async assertIdentityVisible(name: string): Promise<void> {
    await expect(this.#dialog.getByText(name)).toBeVisible();
  }

  async assertIdentityHidden(name: string): Promise<void> {
    await expect(this.#dialog.getByText(name)).toBeHidden();
  }

  async close(): Promise<void> {
    await this.#dialog.getByRole("button", { name: "Close" }).click();
  }
}

class IdentitySwitcherPopover {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signOut(): Promise<void>;
  async signOut<T>(
    fn: (confirmation: SignOutConfirmation) => Promise<T>,
  ): Promise<T>;
  async signOut<T>(
    fn?: (confirmation: SignOutConfirmation) => Promise<T>,
  ): Promise<T | void> {
    await this.#page
      .getByRole("group")
      .getByRole("button", { name: "Sign Out" })
      .click();
    await expect(
      this.#page.getByRole("heading", {
        name: "Sign out from this device",
      }),
    ).toBeVisible();
    const confirmation = new SignOutConfirmation(this.#page);
    if (fn) {
      return fn(confirmation);
    }
    await confirmation.keepIdentity();
  }

  async manageIdentities<T>(
    fn: (dialog: ManageIdentitiesDialog) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Edit" }).click();
    const dialog = this.#page.getByRole("dialog");
    await expect(
      dialog.getByRole("heading", { name: "Identities on this device" }),
    ).toBeVisible();
    const manageDialog = new ManageIdentitiesDialog(dialog);
    const value = await fn(manageDialog);
    await expect(dialog).toBeHidden();
    return value;
  }
}

class ManagePage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signOut(): Promise<void>;
  async signOut<T>(
    fn: (confirmation: SignOutConfirmation) => Promise<T>,
  ): Promise<T>;
  async signOut<T>(
    fn?: (confirmation: SignOutConfirmation) => Promise<T>,
  ): Promise<T | void> {
    return await this.openIdentitySwitcher((switcher) =>
      switcher.signOut(fn as never),
    );
  }

  async openIdentitySwitcher<T>(
    fn: (popover: IdentitySwitcherPopover) => Promise<T>,
  ): Promise<T> {
    await this.#page.getByRole("button", { name: "Switch identity" }).click();
    const popover = new IdentitySwitcherPopover(this.#page);
    return fn(popover);
  }

  async assertVisible() {
    await this.#page.waitForURL(
      (url) => url.origin === II_URL && url.pathname === "/manage",
    );
    await expect(
      this.#page.getByRole("heading", { name: "Welcome" }),
    ).toBeVisible();
    await expect(
      this.#page.getByText("Your identity and sign-in methods at a glance."),
    ).toBeVisible();
  }
}

export const test = base.extend<{
  managePage: ManagePage;
}>({
  managePage: async ({ page }, use) => {
    await use(new ManagePage(page));
  },
});
