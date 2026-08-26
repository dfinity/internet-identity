import { expect } from "@playwright/test";
import { test } from "../../../fixtures";
import {
  openSettings,
  SESSION_SIGN_IN,
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
  test.use({ authorizeConfig: SESSION_SIGN_IN });

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
        await expect(
          settings.getByRole("button", { name: "Sign out" }).first(),
        ).toBeVisible();
        await settings.close();
      },
    );

    test("picks an identity and continues", signInAsFirstIdentity);
  });
});
