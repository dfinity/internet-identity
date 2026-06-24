/**
 * Test fixture for the verified-emails feature.
 *
 * Owns the panel-side surface: opening the wizard from the manage-page
 * "Verified emails" panel, asserting the wizard's nonce uses the new
 * `II-Verify-` prefix, asserting the list / cap / remove flows. The
 * heavy DNSSEC + DKIM + DoH machinery is shared with the recovery
 * flow — tests that go end-to-end pull in the existing `emailRecovery`
 * fixture for that side, since both fixtures attach to the same Page.
 *
 * Same recipient (`register@id.ai`) and same submit_email helper as
 * the recovery setup leg — what differs is the Subject prefix the
 * canister issues (`II-Verify-` instead of `II-Recovery-`), which
 * `PendingKind` uses to dispatch the inbound to the right storage.
 */

import { test as base, expect, Locator, Page } from "@playwright/test";
import { II_URL } from "../utils";

class VerifiedEmailWizard {
  readonly #view: Locator;

  constructor(view: Locator) {
    this.#view = view;
  }

  async enterAddress(address: string): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Verify your email address" }),
    ).toBeVisible();
    await this.#view
      .getByRole("textbox", { name: "Email address" })
      .fill(address);
    await this.#view.getByRole("button", { name: "Continue" }).click();
  }

  /** Step 2: assert the verify-email view rendered with the right
   *  recipient and an `II-Verify-` nonce in the Subject. `exact` so
   *  this matcher doesn't bleed onto the step-1 "Verify your email
   *  address" heading. */
  async expectVerifyEmailView(): Promise<void> {
    await expect(
      this.#view.getByRole("heading", {
        name: "Verify your email",
        exact: true,
      }),
    ).toBeVisible();
    await expect(this.#view.getByText("register@id.ai")).toBeVisible();
    await expect(this.#view.getByText(/II-Verify-[0-9a-f]{16}/)).toBeVisible();
  }

  /** Pull the canister-issued `II-Verify-<hex>` token off the rendered
   *  Subject block so the caller can submit a matching DKIM email. */
  async readNonce(): Promise<string> {
    const text = await this.#view
      .getByText(/II-Verify-[0-9a-f]{16}/)
      .textContent();
    if (text === null) {
      throw new Error("verified-email wizard did not render a nonce");
    }
    return text;
  }

  /** Step 3: the wizard parks on a success card after the canister
   *  reports `RegistrationSucceeded`. Click Done to fire the host's
   *  `onSuccess` (toast + close). */
  async confirmSuccess(): Promise<void> {
    await expect(
      this.#view.getByRole("heading", { name: "Email verified" }),
    ).toBeVisible({ timeout: 30_000 });
    await this.#view.getByRole("button", { name: "Done" }).click();
  }

  async close(): Promise<void> {
    await this.#view.getByRole("button", { name: "Close" }).click();
  }
}

class VerifiedEmailFixtures {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  /** Navigate to the /manage/communication page where the
   *  verified-emails panel now lives (sibling of /manage/recovery
   *  after the Communication-page split). */
  async goto(): Promise<void> {
    await this.#page.goto(II_URL + "/manage/communication");
  }

  /** The "Shareable info" page sits on its own top-level route under
   *  /manage; assert the page header and the panel rendered. */
  async assertPanelVisible(): Promise<void> {
    await expect(
      this.#page.getByRole("heading", { name: "Shareable info", level: 1 }),
    ).toBeVisible();
    await expect(
      this.#page.getByRole("heading", { name: "Email addresses", level: 2 }),
    ).toBeVisible();
    await expect(
      this.#page.getByRole("button", { name: "Add an email" }),
    ).toBeVisible();
  }

  /** Open the verify-email wizard from the panel and run `fn` against
   *  it. Mirrors `emailRecovery.openSetupWizard` for ergonomics. */
  async openWizard<T>(
    fn: (wizard: VerifiedEmailWizard) => Promise<T>,
  ): Promise<T> {
    await this.#page
      .getByRole("main")
      .getByRole("button", { name: "Add an email" })
      .click();
    const dialog = this.#page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    return fn(new VerifiedEmailWizard(dialog));
  }

  /** Assert that the panel's list contains a row for `address`. */
  async assertAddressListed(address: string): Promise<void> {
    const panel = this.#panel();
    await expect(panel.getByText(address, { exact: true })).toBeVisible();
  }

  async assertAddressAbsent(address: string): Promise<void> {
    const panel = this.#panel();
    await expect(panel.getByText(address, { exact: true })).toBeHidden();
  }

  /** Click the row's direct trash-icon button and confirm in the
   *  confirmation dialog. */
  async removeAddress(address: string): Promise<void> {
    const panel = this.#panel();
    const row = panel.locator("li").filter({
      has: this.#page.getByText(address, { exact: true }),
    });
    await row.getByRole("button", { name: `Remove ${address}` }).click();
    const confirm = this.#page.getByRole("dialog");
    await expect(confirm).toBeVisible();
    await confirm.getByRole("button", { name: "Remove", exact: true }).click();
    await expect(confirm).toBeHidden();
  }

  /** Locator scoped to the "Email addresses" panel section on the
   *  Communication page. */
  #panel(): Locator {
    return this.#page.locator("section").filter({
      has: this.#page.getByRole("heading", { name: "Email addresses" }),
    });
  }
}

export const test = base.extend<{
  verifiedEmail: VerifiedEmailFixtures;
}>({
  verifiedEmail: async ({ page }, use) => {
    await use(new VerifiedEmailFixtures(page));
  },
});
