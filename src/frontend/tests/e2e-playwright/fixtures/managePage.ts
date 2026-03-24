import { Page, test as base, expect } from "@playwright/test";
import { II_URL } from "../utils";

class ManagePage {
  #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  async signOut() {
    await this.#page.getByRole("button", { name: "Switch identity" }).click();
    await this.#page
      .getByRole("group")
      .getByRole("button", { name: "Sign Out" })
      .click();
    await this.#page.waitForURL(II_URL); // Wait for redirect to landing page after sign out
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
