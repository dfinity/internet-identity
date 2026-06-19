/**
 * Coverage for the verified-emails feature (Phase 1).
 *
 * Two layers:
 *
 *   1. **Wizard surface** — the "Verify an email" button on the
 *      manage-page panel opens the wizard, the wizard requests the
 *      address, and the canister-issued nonce uses the new
 *      `II-Verify-` prefix.
 *
 *   2. **Real end-to-end** — same DNSSEC + DKIM + DoH machinery as
 *      the recovery flow (reused from the `emailRecovery` fixture).
 *      The `register@id.ai` recipient is shared with the recovery
 *      setup leg; what disambiguates this from a recovery
 *      registration is the Subject prefix the canister issues
 *      (`II-Verify-` → `PendingKind::VerifyEmail`), which lands in
 *      `Anchor.verified_emails` rather than `Anchor.email_recovery`.
 *
 * The cap (`MAX_VERIFIED_EMAILS_PER_ANCHOR = 5`) is enforced
 * backend-side; integration tests in `verified_emails.rs` cover the
 * cap behaviour end-to-end. Here we just assert the panel renders
 * the counter and the recovery card remains untouched when a
 * verified email is added.
 */

import { test } from "../fixtures";
import { expect } from "@playwright/test";

const STATUS_POLL_TIMEOUT = 30_000;

test.describe("Verified emails — wizard surface", () => {
  test("manage page renders the panel and opens the wizard", async ({
    page,
    verifiedEmail,
    manageRecoveryPage,
    signInWithIdentity,
    identities,
  }) => {
    await manageRecoveryPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await verifiedEmail.assertPanelVisible();

    await page
      .getByRole("main")
      .getByRole("button", { name: "Verify an email" })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Verify an email" }),
    ).toBeVisible();
    await dialog.getByRole("button", { name: "Close" }).click();
    await expect(dialog).toBeHidden();
  });
});

test.describe("Verified emails — real DNSSEC + DKIM flow", () => {
  test("add a verified email via the panel, then remove it", async ({
    page,
    verifiedEmail,
    emailRecovery,
    manageRecoveryPage,
    signInWithIdentity,
    identities,
  }) => {
    test.slow(); // RSA keygen + DNSSEC walk + status polling

    // The DoH interceptor and DKIM signer are shared with the recovery
    // flow — the verified-email path only differs in the Subject prefix
    // the canister issues, which `PendingKind` uses to dispatch.
    await emailRecovery.installDohInterceptor();

    await manageRecoveryPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await verifiedEmail.assertPanelVisible();

    const address = emailRecovery.fromAddress;
    await verifiedEmail.openWizard(async (wizard) => {
      await wizard.enterAddress(address);
      await wizard.expectVerifyEmailView();
      const nonce = await wizard.readNonce();
      await emailRecovery.submitEmail({
        to: "register@id.ai",
        subject: nonce,
      });
      // Wizard closes when the polling status flips to
      // RegistrationSucceeded (reused for verified-email completion);
      // the host fires its toast and reloads the layout data so the
      // new row paints.
    });

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeHidden({ timeout: STATUS_POLL_TIMEOUT });
    await verifiedEmail.assertAddressListed(address);

    // ---------------------------------------------------------------
    // Recovery card stays untouched — the recovery-email setup CTA is
    // still in its "Activate recovery email" state.
    // ---------------------------------------------------------------
    await emailRecovery.assertSetupCardVisible();

    // ---------------------------------------------------------------
    // Remove the verified email — confirmation dialog, list updates.
    // ---------------------------------------------------------------
    await verifiedEmail.removeAddress(address);
    await verifiedEmail.assertAddressAbsent(address);
  });
});
