import { test as base, expect, type Page } from "@playwright/test";

export const DEFAULT_OPENID_PORT = 11105;
export const ALTERNATE_OPENID_PORT = 11106;

export type OpenIdConfig = {
  defaultPort?: number;
  createUsers: Array<{
    port?: number;
    claims?: Record<string, unknown>;
  }>;
};

export type OpenIdUser = {
  id: string;
  issuer: { name: string; url: string };
  claims: Record<string, unknown>;
};

export const test = base.extend<{
  openIdConfig: OpenIdConfig;
  openIdUsers: OpenIdUser[];
  signInWithOpenId: (page: Page, userId: string) => Promise<void>;
}>({
  openIdConfig: {
    defaultPort: DEFAULT_OPENID_PORT,
    createUsers: [
      {
        claims: { name: "John Doe", email: "john.doe@example.com" },
      },
    ],
  },
  openIdUsers: async ({ openIdConfig }, use) => {
    const defaultPort = openIdConfig.defaultPort ?? DEFAULT_OPENID_PORT;
    const users: OpenIdUser[] = openIdConfig.createUsers.map((config) => ({
      id: crypto.randomUUID(),
      issuer: {
        name: `Test OpenID ${config.port ?? defaultPort}`,
        url: `http://localhost:${config.port ?? defaultPort}`,
      },
      claims: config.claims ?? {},
    }));
    await Promise.all(
      users.map((user) =>
        fetch(`${user.issuer.url}/account/${user.id}/claims`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(user.claims ?? {}),
        }),
      ),
    );
    await use(users);
  },
  signInWithOpenId: async ({ openIdUsers }, use) => {
    await use(async (page: Page, userId?: string) => {
      const user =
        userId === undefined
          ? openIdUsers[0]
          : openIdUsers.find((u) => u.id === userId);
      if (user === undefined) {
        throw new Error("User not found");
      }
      await expect(
        page.getByRole("heading", {
          name: "Sign-in",
        }),
      ).toBeVisible();
      await page.getByPlaceholder("Enter any login").fill(user.id);
      await page.getByPlaceholder("and password").fill("any-password-works");
      await page.getByRole("button", { name: "Sign-in" }).click();
      await expect(
        page.getByRole("heading", {
          name: "Authorize",
        }),
      ).toBeVisible();
      await page.getByRole("button", { name: "Continue" }).click();
    });
  },
});
