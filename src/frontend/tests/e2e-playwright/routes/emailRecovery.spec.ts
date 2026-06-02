/**
 * Coverage for the email-recovery feature, two layers deep:
 *
 *   1. **Wizard surface** — flag gating + dialog open/close. Catches
 *      regressions in the FE wiring without needing a working
 *      DNSSEC chain or DKIM key.
 *
 *   2. **Real end-to-end** — synthesizes a DNSSEC chain
 *      (`utils/dnssecTestSigner.ts`) plus a DKIM keypair, intercepts
 *      the FE's DoH lookups, drives the wizard to the
 *      send-confirmation-email step, and submits a DKIM-signed
 *      `smtp_request` so polling
 *      flips to `RegistrationSucceeded` / `RecoveryReady`. Exercises
 *      the canister's DNSSEC verifier + DKIM/DMARC verifier + status
 *      transitions end-to-end. The local II canister is deployed
 *      with a trust anchor matching the signer's deterministic seed
 *      (see `local_test_arg.did` + `.github/workflows/canister-tests.yml`).
 *
 * Both layers share one fixture (`emailRecovery`); the heavy DNSSEC
 * + DKIM material is built lazily so gating tests stay fast.
 */

import { test } from "../fixtures";
import { expect } from "@playwright/test";
import { II_URL } from "../utils";

// The canister gives ~30 minutes for the email to arrive but the
// test budget is much smaller. We submit immediately after the
// wizard transitions, then wait for the polling status flip.
const STATUS_POLL_TIMEOUT = 30_000;

test.describe("Email recovery — feature-flag gating", () => {
  test("manage page hides the email card when the flag is off", async ({
    page,
    emailRecovery,
    signInWithIdentity,
    identities,
  }) => {
    await emailRecovery.disableFlag();
    await page.goto(II_URL + "/manage/recovery");
    await signInWithIdentity(page, identities[0].identityNumber);
    await emailRecovery.assertSetupCardHidden();
  });

  test("recover sign-in hides the email button when the flag is off", async ({
    page,
    emailRecovery,
  }) => {
    await emailRecovery.disableFlag();
    await page.goto(II_URL + "/recovery");
    await emailRecovery.assertRecoverWithEmailHidden();
  });
});

test.describe("Email recovery — wizard surface", () => {
  test("manage page renders the inactive card and opens the wizard", async ({
    page,
    emailRecovery,
    manageRecoveryPage,
    signInWithIdentity,
    identities,
  }) => {
    await emailRecovery.enableFlag();
    await manageRecoveryPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await emailRecovery.assertSetupCardVisible();

    await page
      .getByRole("main")
      .getByRole("button", { name: "Activate recovery email" })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Add a recovery email" }),
    ).toBeVisible();
    // Wizard views no longer carry a Cancel button — the dialog's
    // built-in close (X) is the only user-driven exit. See
    // setupEmailRecovery/views/EnterAddress.svelte.
    await dialog.getByRole("button", { name: "Close" }).click();
    await expect(dialog).toBeHidden();
  });

  test("recover sign-in shows the email button and opens the wizard", async ({
    page,
    emailRecovery,
  }) => {
    await emailRecovery.enableFlag();
    await page.goto(II_URL + "/recovery");
    await expect(
      page.getByRole("button", { name: "Recover with email" }),
    ).toBeVisible();

    await page.getByRole("button", { name: "Recover with email" }).click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Recover with email" }),
    ).toBeVisible();
    await dialog.getByRole("button", { name: "Close" }).click();
    await expect(dialog).toBeHidden();
  });
});

test.describe("Email recovery — real DNSSEC + DKIM flow", () => {
  test("setup then recover an identity via email", async ({
    page,
    emailRecovery,
    manageRecoveryPage,
    signInWithIdentity,
    identities,
  }) => {
    test.slow(); // RSA keygen + DNSSEC walk + status polling

    await emailRecovery.enableFlag();
    await emailRecovery.installDohInterceptor();

    // ---------------------------------------------------------------
    // Setup leg — bind alice@test.example.com as a recovery method
    // ---------------------------------------------------------------
    await manageRecoveryPage.goto();
    await signInWithIdentity(page, identities[0].identityNumber);
    await emailRecovery.assertSetupCardVisible();

    await page
      .getByRole("main")
      .getByRole("button", { name: "Activate recovery email" })
      .click();
    const setupDialog = page.getByRole("dialog");
    await expect(setupDialog).toBeVisible();
    await setupDialog
      .getByRole("textbox", { name: "Email address" })
      .fill(emailRecovery.fromAddress);
    await setupDialog.getByRole("button", { name: "Continue" }).click();

    // The wizard is now polling email_recovery_status. Pull the
    // canister-issued nonce off the rendered token block, sign an
    // email with the matching subject, submit it via smtp_request.
    await expect(
      setupDialog.getByRole("heading", { name: "Verify your email" }),
    ).toBeVisible();
    const setupNonce = await setupDialog
      .getByText(/II-Recovery-[0-9a-f]{16}/)
      .textContent();
    if (setupNonce === null) {
      throw new Error("setup wizard did not render a nonce");
    }
    await emailRecovery.submitEmail({
      to: "register@id.ai",
      subject: setupNonce,
    });

    // On RegistrationSucceeded the wizard fires `onSuccess` which the
    // host translates into a toast + closing the dialog. Assert the
    // dialog goes away and the active recovery-email card now shows
    // the bound address (the inactive card variant doesn't). Use
    // exact-text match because the address also appears as a
    // substring inside the success toast's description.
    await expect(setupDialog).toBeHidden({ timeout: STATUS_POLL_TIMEOUT });
    await expect(
      page.getByText(emailRecovery.fromAddress, { exact: true }),
    ).toBeVisible();

    // ---------------------------------------------------------------
    // Recovery leg — sign in via the email we just bound
    // ---------------------------------------------------------------
    // The DoH interceptor was installed page-wide and persists across
    // navigations — no need to reinstall.
    await page.goto(II_URL + "/recovery");

    await page.getByRole("button", { name: "Recover with email" }).click();
    const recoverDialog = page.getByRole("dialog");
    await expect(recoverDialog).toBeVisible();
    await recoverDialog
      .getByRole("textbox", { name: "Email address" })
      .fill(emailRecovery.fromAddress);
    await recoverDialog.getByRole("button", { name: "Continue" }).click();

    await expect(
      recoverDialog.getByRole("heading", { name: "Verify your email" }),
    ).toBeVisible();
    const recoveryNonce = await recoverDialog
      .getByText(/II-Recovery-[0-9a-f]{16}/)
      .textContent();
    if (recoveryNonce === null) {
      throw new Error("recovery wizard did not render a nonce");
    }
    await emailRecovery.submitEmail({
      to: "recover@id.ai",
      subject: recoveryNonce,
    });

    // On success the wizard issues a delegation and the host page
    // navigates to /manage/access with a success toast. Assert the
    // destination heading rather than the toast (auto-dismisses).
    await page.waitForURL(/\/manage\/access/, {
      timeout: STATUS_POLL_TIMEOUT,
    });
    await expect(
      page.getByRole("heading", { name: /access methods/i }).first(),
    ).toBeVisible();
  });
});
