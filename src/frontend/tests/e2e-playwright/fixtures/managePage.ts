import { expect, Page, test as base } from "@playwright/test";
import { II_URL } from "../utils";
import { Identity } from "./identity";

class Navigation {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async home() {
    await this.#openMenuOnMobile();
    await this.#page
      .getByRole("complementary")
      .getByRole("link", { name: "Home" })
      .click();
  }

  async assertHome(name: string) {
    await expect(
      this.#page
        .getByRole("main")
        .getByRole("heading", { name: `Welcome, ${name}!` }),
    ).toBeVisible();
    await expect(this.#page).toHaveURL(II_URL + "/manage");
  }

  async accessMethods() {
    await this.#accessAndRecovery();
    await this.#page
      .getByRole("main")
      .getByRole("navigation")
      .getByRole("link", { name: "Access methods" })
      .click();
  }

  async assertAccessMethods() {
    await expect(
      this.#page
        .getByRole("main")
        .getByRole("heading", { name: "Access methods" }),
    ).toBeVisible();
    await expect(this.#page).toHaveURL(II_URL + "/manage");
  }

  async recoveryPhrase() {
    await this.#accessAndRecovery();
    await this.#page
      .getByRole("main")
      .getByRole("navigation")
      .getByRole("link", { name: "Recovery phrase" })
      .click();
  }

  async assertRecoveryPhrase() {
    await expect(
      this.#page
        .getByRole("main")
        .getByRole("heading", { name: "Recovery phrase" }),
    ).toBeVisible();
    await expect(this.#page).toHaveURL(II_URL + "/manage/recovery");
  }

  async #accessAndRecovery() {
    await this.#openMenuOnMobile();
    await this.#page
      .getByRole("complementary")
      .getByRole("link", { name: "Access and recovery" })
      .click();
  }

  async #openMenuOnMobile() {
    const menuButton = this.#page
      .getByRole("banner")
      .getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
  }
}

class ManagePage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async goto() {
    await this.#page.goto(II_URL + "/manage");
  }

  async switchToIdentity(identity: Identity) {
    const menuButton = this.#page
      .getByRole("banner")
      .getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
    await this.#page.getByRole("button", { name: "Identity switcher" }).click();
    identity.auth(this.#page);
    await this.#page.getByRole("button", { name: identity.name }).click();
  }

  async signOut() {
    const menuButton = this.#page
      .getByRole("banner")
      .getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
    await this.#page.getByRole("button", { name: "Identity switcher" }).click();
    await this.#page.getByRole("button", { name: "Sign out" }).click();
  }

  async navigation(fn: (navigation: Navigation) => Promise<void>) {
    await fn(new Navigation(this.#page));
  }
}

export const test = base.extend<{
  managePage: ManagePage;
}>({
  managePage: async ({ page }, use) => {
    await use(new ManagePage(page));
  },
});
