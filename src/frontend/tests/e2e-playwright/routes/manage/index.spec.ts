import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { II_URL } from "../../utils";

test.describe("Session re-authentication", () => {
  test("Shows re-auth dialog after session timeout and re-authenticates", async ({
    page,
    identities,
    signInWithIdentity,
    managePage,
  }) => {
    // Install fake timers before navigating so we can fast-forward
    await page.clock.install();
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    // Fast-forward 25 minutes to trigger the re-auth dialog
    await page.clock.fastForward(25 * 60 * 1000);

    // Re-auth dialog should appear
    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(
      dialog.getByRole("heading", { name: "Session timed out" }),
    ).toBeVisible();
    await expect(
      dialog.getByText("Sign in again to continue where you left off."),
    ).toBeVisible();

    // Resume real time before re-authenticating: `fastForward` only fires
    // timers due at the moment it's called, so any timer scheduled *during*
    // the re-authentication network flow below (e.g. IC agent retry/backoff)
    // would otherwise never fire under a still-frozen clock.
    await page.clock.resume();

    // Click sign in to re-authenticate
    await dialog.getByRole("button", { name: "Sign in" }).click();

    // Dialog should close after successful re-auth. Re-authentication makes a
    // real IC call (device-key lookup) plus a WebAuthn assertion, which can
    // comfortably exceed the default 5s expect timeout under CI load; give it
    // more headroom rather than tightly coupling the test to call latency.
    await expect(dialog).toBeHidden({ timeout: 15_000 });

    // User should still be on the manage page
    await managePage.assertVisible();
  });

  test("Shows re-auth dialog and allows cancelling to sign out", async ({
    page,
    identities,
    signInWithIdentity,
    managePage,
  }) => {
    await page.clock.install();
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    // Fast-forward 25 minutes to trigger the re-auth dialog
    await page.clock.fastForward(25 * 60 * 1000);

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();

    // Click cancel to sign out
    await dialog.getByRole("button", { name: "Cancel" }).click();

    // Should redirect to home page
    await page.waitForURL(II_URL);
  });
});

test.describe("Dashboard Navigation", () => {
  test("User can register, sign in, access the dashboard and navigate to security page", async ({
    page,
    identities,
    signInWithIdentity,
    managePage,
  }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    // Navigate to access methods
    const menuButton = page.getByRole("button", { name: "Open menu" });
    if (await menuButton.isVisible()) {
      await menuButton.click();
    }
    await page.getByRole("link", { name: "Access" }).click();

    // Check that we have one passkey listed
    const passkey = await page.getByText("Unknown");
    await expect(passkey).toBeVisible();
    await expect(passkey).toHaveCount(1);
  });

  test.describe("multiple identities", () => {
    test.use({
      identityConfig: {
        createIdentities: [{ name: "Test 1" }, { name: "Test 2" }],
      },
    });

    test.beforeEach(
      async ({
        page,
        managePage,
        identities,
        signInWithIdentity,
        removeAuthenticatorForIdentity,
      }) => {
        // Sign in with both identities to add them both to switcher
        await page.goto(II_URL);
        await signInWithIdentity(page, identities[1].identityNumber);
        await managePage.signOut();
        await removeAuthenticatorForIdentity(identities[1].identityNumber);
        await signInWithIdentity(page, identities[0].identityNumber);
        await managePage.signOut();
        await removeAuthenticatorForIdentity(identities[0].identityNumber);
      },
    );

    test("User can switch between identities", async ({
      page,
      managePage,
      identities,
      addAuthenticatorForIdentity,
      removeAuthenticatorForIdentity,
    }) => {
      // Sign in to dashboard with first identity
      await page.goto(II_URL);
      await addAuthenticatorForIdentity(page, identities[0].identityNumber);
      await page.getByRole("button", { name: "Continue", exact: true }).click();

      // Verify we're at the dashboard and signed in as the first identity
      await managePage.assertVisible();
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${identities[0].name}\\.`),
        }),
      ).toBeVisible();

      // Navigate to access methods
      const menuButton = page.getByRole("button", { name: "Open menu" });
      if (await menuButton.isVisible()) {
        await menuButton.click();
      }
      await page.getByRole("link", { name: "Access" }).click();

      // Switch to second identity
      await removeAuthenticatorForIdentity(identities[0].identityNumber);
      await addAuthenticatorForIdentity(page, identities[1].identityNumber);
      await page.getByRole("button", { name: "Switch identity" }).click();
      await page.getByRole("button", { name: identities[1].name }).click();

      // Verify we're back at the dashboard homepage and signed in as the second identity
      await managePage.assertVisible();
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${identities[1].name}\\.`),
        }),
      ).toBeVisible();
    });
  });
});
