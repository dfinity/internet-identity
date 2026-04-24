import { expect, Locator, Page } from "@playwright/test";
import { test as authorizeTest } from "./authorize";

/** Page-object for the explicit ICRC-3 attribute consent screen. Wraps the
 *  II authorize window so specs don't hand-roll role queries every time. */
class AttributeConsentView {
  // `AttributeConsentView.svelte` renders a different heading depending on
  // the flow variant: "Review Permissions" for OpenID 1-click and "Allow to
  // access this info" for the regular authorize flow. Match either so tests
  // don't need to know which variant they triggered.
  static readonly HEADING_RE =
    /^(Review Permissions|Allow to access this info)$/;

  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  get heading(): Locator {
    return this.#page.getByRole("heading", {
      name: AttributeConsentView.HEADING_RE,
    });
  }

  /** A single attribute row, matched by its label (e.g. `"Email:"`,
   *  `"Google email:"`).
   *
   *  aria-labels wrap runtime-interpolated values (provider names,
   *  attribute keys) in Unicode bidi isolation marks (FSI U+2068 / PDI
   *  U+2069) for correct RTL rendering, so a plain-substring match won't
   *  hit those rows. The regex below lets any character in the caller's
   *  label be preceded/followed by those marks, and anchors at both ends
   *  so `"Email:"` doesn't accidentally match `"Google email:"`. */
  row(label: string): Locator {
    const body = label
      .split("")
      .map((c) => c.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
      .join("[\\u2068\\u2069]*");
    const pattern = new RegExp(
      `^[\\u2068\\u2069]*${body}[\\u2068\\u2069]*$`,
      "i",
    );
    return this.#page.getByRole("group", { name: pattern });
  }

  /** All rendered attribute rows — useful for counting. */
  get rows(): Locator {
    return this.#page.getByRole("group");
  }

  /** The picker's chevron button; only present when a row has >1 option. */
  get pickerButton(): Locator {
    return this.#page.getByRole("button", { name: "Change" });
  }

  async waitForVisible(): Promise<void> {
    await expect(this.heading).toBeVisible();
  }

  async expectHidden(): Promise<void> {
    await expect(this.heading).toBeHidden();
  }

  async uncheckRow(label: string): Promise<void> {
    await this.row(label).getByRole("checkbox").uncheck();
  }

  async denyAll(): Promise<void> {
    await this.#page.getByRole("button", { name: "Deny All" }).click();
  }

  async continue(): Promise<void> {
    await this.#page.getByRole("button", { name: "Continue" }).click();
  }

  /** Convenience: wait for the screen and click Continue. */
  async accept(): Promise<void> {
    await this.waitForVisible();
    await this.continue();
  }
}

export const test = authorizeTest.extend<{
  attributeConsentView: AttributeConsentView;
}>({
  attributeConsentView: async ({ authorizePage }, use) => {
    await use(new AttributeConsentView(authorizePage.page));
  },
});
