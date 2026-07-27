import { expect } from "@playwright/test";
import { II_URL } from "../utils";
import { test } from "../fixtures";

test.describe("Sign-in ↔ sign-up modal toggle on returning-user landing", () => {
  test.use({
    identityConfig: {
      createIdentities: [{ name: "Alice" }],
    },
  });

  test.beforeEach(
    async ({ page, identities, signInWithIdentity, managePage }) => {
      await page.goto(II_URL);
      await signInWithIdentity(page, identities[0].identityNumber);
      await managePage.signOut((c) => c.keepIdentity());
    },
  );

  test("clicking Create CTA inside sign-in modal opens sign-up modal and closes sign-in modal", async ({
    page,
  }) => {
    // Open the sign-in modal from "Add identity" on the returning-user landing
    await page.getByRole("button", { name: "Add identity" }).click();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeVisible();

    // Click the "Create" toggle CTA inside the sign-in modal
    await page.getByRole("button", { name: "Create", exact: true }).click();

    // Sign-up modal should be visible; sign-in heading should be gone
    await expect(page.getByRole("dialog")).toBeVisible();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeHidden();
  });

  test("clicking Sign in CTA inside sign-up modal returns to sign-in modal", async ({
    page,
  }) => {
    // Open sign-in modal, then toggle to sign-up
    await page.getByRole("button", { name: "Add identity" }).click();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeVisible();
    await page.getByRole("button", { name: "Create", exact: true }).click();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeHidden();

    // Click "Sign in" inside the sign-up modal — should return to sign-in modal
    await page.getByRole("button", { name: "Sign in", exact: true }).click();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeVisible();
  });

  test("dismissing the sign-up modal closes it without re-opening sign-in modal", async ({
    page,
  }) => {
    await page.getByRole("button", { name: "Add identity" }).click();
    await page.getByRole("button", { name: "Create", exact: true }).click();

    // Close via the accessible Close button
    await page.getByRole("button", { name: "Close" }).click();

    // No dialog open; landing page is visible again
    await expect(page.getByRole("dialog")).toBeHidden();
    await expect(
      page.getByRole("heading", { name: "Add existing identity" }),
    ).toBeHidden();
    await expect(
      page.getByRole("button", { name: "Add identity" }),
    ).toBeVisible();
  });
});
