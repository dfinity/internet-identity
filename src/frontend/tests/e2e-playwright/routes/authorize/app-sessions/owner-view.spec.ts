import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import {
  listedBrowserRows,
  openSettings,
  signInAsFirstIdentity,
} from "./helpers";

/**
 * Ending access is only useful if the owner can see what there is to end, so a
 * sign-in has to show up in the identity's own settings.
 *
 * Runs SHOW-2 of `docs/ongoing/session-test-scenarios.md`. The rest of that group
 * asks what the list says rather than whether it is there, which the settings
 * specs cover.
 */
test.describe("what the owner sees", () => {
  test.use({ authorizeConfig: { protocol: "icrc25" } });

  test.describe("the browser appears in the list after signing in", () => {
    test.afterEach(
      async ({ testApp, context, identities, signInWithIdentity }) => {
        // Stated here rather than taken from `signedInApp`, which this hook has
        // nothing else to read: the row exists only once the sign-in has reached
        // the canister.
        await testApp.waitUntilSignedIn();

        const settings = await openSettings(
          context,
          identities[0].identityNumber,
          signInWithIdentity,
        );

        // SHOW-2 promises a name that identifies the browser and when it was
        // last used, so both are read. A visible button would pass for a row
        // that named nothing.
        const row = listedBrowserRows(settings).first();
        await expect(row).toBeVisible();
        // Chromium is what Playwright drives, and the list names a browser by
        // its brand: anything else means the description never reached the
        // canister, or came back unrecognised.
        await expect(settings.getByText(/Chrom(e|ium)/).first()).toBeVisible();
        // "Right now" is what a browser in use reads as — the stamp is only
        // five-minute precise, so a browser that just signed in is inside the
        // first grain.
        await expect(settings.getByText("Right now").first()).toBeVisible();
        await settings.close();
      },
    );

    test("picks an identity and continues", signInAsFirstIdentity);
  });
});
