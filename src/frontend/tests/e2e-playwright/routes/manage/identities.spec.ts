import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import { II_URL } from "../../utils";

test.describe("Manage identities", () => {
  test.use({
    identityConfig: {
      createIdentities: [{ name: "Alice" }, { name: "Bob" }, { name: "Carol" }],
    },
  });

  test.beforeEach(
    async ({ page, managePage, identities, signInWithIdentity }) => {
      // Sign in with all identities to populate the switcher
      await page.goto(II_URL);
      await signInWithIdentity(page, identities[2].identityNumber);
      await managePage.signOut((c) => c.keepIdentity());
      await signInWithIdentity(page, identities[1].identityNumber);
      await managePage.signOut((c) => c.keepIdentity());
      await signInWithIdentity(page, identities[0].identityNumber);
      await managePage.assertVisible();
    },
  );

  test("can remove an identity and undo", async ({
    page,
    managePage,
    identities,
  }) => {
    await managePage.openIdentitySwitcher(async (switcher) => {
      await switcher.manageIdentities(async (dialog) => {
        await dialog.remove(identities[1].name, async (confirmation) => {
          await confirmation.confirm();
        });
      });
    });

    // Wait for toast and undo the removal
    const toast = page.getByRole("status");
    await expect(toast).toContainText("Identity removed");
    await expect(toast).toContainText(
      `${identities[1].name} has been removed from this device.`,
    );
    await toast.getByRole("button", { name: "Undo" }).click();

    // Verify identity is back in the switcher
    await managePage.openIdentitySwitcher(async (switcher) => {
      await switcher.manageIdentities(async (dialog) => {
        await dialog.assertIdentityVisible(identities[1].name);
        await dialog.close();
      });
    });
  });

  test("removed identity disappears from switcher", async ({
    page,
    managePage,
    identities,
  }) => {
    await managePage.openIdentitySwitcher(async (switcher) => {
      await switcher.manageIdentities(async (dialog) => {
        await dialog.remove(identities[1].name, async (confirmation) => {
          await confirmation.confirm();
        });
      });
    });

    // Dismiss the toast
    const toast = page.getByRole("status");
    await expect(toast).toContainText("Identity removed");
    await expect(toast).toContainText(
      `${identities[1].name} has been removed from this device.`,
    );
    await toast.getByRole("button", { name: "Close" }).click();

    // Open switcher and verify identity is gone from the manage dialog
    await managePage.openIdentitySwitcher(async (switcher) => {
      await switcher.manageIdentities(async (dialog) => {
        await dialog.assertIdentityHidden(identities[1].name);
        await dialog.close();
      });
    });
  });

  test("can cancel identity removal", async ({ managePage, identities }) => {
    await managePage.openIdentitySwitcher(async (switcher) => {
      await switcher.manageIdentities(async (dialog) => {
        await dialog.remove(identities[1].name, async (confirmation) => {
          await confirmation.cancel();
        });
        // Should be back at identity list
        await dialog.assertIdentityVisible(identities[1].name);
        await dialog.close();
      });
    });
  });
});

test.describe("Sign out confirmation", () => {
  test("sign out and keep identity", async ({
    page,
    managePage,
    identities,
    signInWithIdentity,
  }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    await managePage.signOut(async (confirmation) => {
      await confirmation.keepIdentity();
    });

    // Identity should still appear on the landing page header
    await expect(
      page.getByRole("button", { name: "Switch identity" }),
    ).toContainText(identities[0].name);
  });

  test("sign out and remove from device", async ({
    page,
    managePage,
    identities,
    signInWithIdentity,
  }) => {
    await page.goto(II_URL);
    await signInWithIdentity(page, identities[0].identityNumber);
    await managePage.assertVisible();

    await managePage.signOut(async (confirmation) => {
      await confirmation.removeFromDevice();
    });

    // No identities left — should see sign in button
    await expect(page.getByRole("button", { name: "Sign in" })).toBeVisible();
  });
});
