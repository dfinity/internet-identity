import { test as base, expect, type Page } from "@playwright/test";

export type OpenIdClaims = Record<string, unknown>;
export type OpenIdUser = {
  id: string;
  claims: OpenIdClaims;
  signIn: (page?: Page) => Promise<void>;
};

export const test = base.extend<{
  openIdIssuer: string;
  openIdClaims: OpenIdClaims;
  openIdUser: OpenIdUser;
}>({
  openIdIssuer: "http://localhost:11105",
  openIdClaims: { name: "John Doe", email: "john.doe@example.com" },
  openIdUser: async ({ openIdIssuer, openIdClaims, page }, use) => {
    const userId = crypto.randomUUID();

    // set claims in the dummy OpenID provider
    await fetch(`${openIdIssuer}/account/${userId}/claims`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(openIdClaims),
    });

    const signIn = async (openIdPage = page) => {
      await openIdPage.waitForURL(`${openIdIssuer}/interaction/*`);
      await expect(
        openIdPage.getByRole("heading", {
          name: "Sign-in",
        }),
      ).toBeVisible();
      await openIdPage.getByPlaceholder("Enter any login").fill(userId);
      await openIdPage
        .getByPlaceholder("and password")
        .fill("any-password-works");
      await openIdPage.getByRole("button", { name: "Sign-in" }).click();
      await expect(
        openIdPage.getByRole("heading", {
          name: "Authorize",
        }),
      ).toBeVisible();
      await openIdPage.getByRole("button", { name: "Continue" }).click();
    };

    await use({ id: userId, claims: openIdClaims, signIn });
  },
});
