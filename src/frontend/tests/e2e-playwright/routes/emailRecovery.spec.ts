/**
 * E2E coverage for the email-recovery feature flag + wizard wiring.
 *
 * What's covered here:
 * - Feature-flag gating on both the manage page and the
 *   recover-sign-in page: when `EMAIL_RECOVERY` is off, the email
 *   surface stays hidden.
 * - Manage-page setup wizard: the "Add email" button opens the
 *   dialog with the right heading.
 * - Recovery-sign-in: the "Recover with email" button is exposed
 *   and opens its dialog.
 *
 * What's NOT covered yet:
 * - Driving the magic-email step end-to-end. That requires the
 *   local II canister to be deployed with a DoH allowlist for the
 *   test domain *and* a way to push a DKIM-signed `smtp_request`
 *   into the canister so the polling status flips. Both are real
 *   work; the canister-side integration tests (PocketIC) already
 *   cover that path. This file exercises the FE wiring (flag
 *   gating, dialog open, view rendering) so a regression in the
 *   wizard's mounting is caught here without rebuilding the
 *   gateway-side test rig.
 */

import { test } from "../fixtures";
import { expect } from "@playwright/test";
import { II_URL } from "../utils";

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
      .getByRole("button", { name: "Add email" })
      .click();
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Add a recovery email" }),
    ).toBeVisible();
    await dialog.getByRole("button", { name: "Cancel" }).click();
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
    await dialog.getByRole("button", { name: "Cancel" }).click();
    await expect(dialog).toBeHidden();
  });
});
