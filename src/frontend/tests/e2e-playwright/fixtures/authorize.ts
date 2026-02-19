import { Principal } from "@icp-sdk/core/principal";
import { test as base, expect, type Page } from "@playwright/test";

export type AuthorizeConfig = {
  testAppURL: string;
  internetIdentityURL: string;
  protocol: "legacy" | "icrc25";
} & (
  | { protocol: "legacy" }
  | { protocol: "icrc25"; openid?: string; attributes?: string[] }
);

class AuthorizePage {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  get page(): Page {
    return this.#page;
  }
}

export const test = base.extend<{
  authorizeConfig: Partial<AuthorizeConfig> | undefined;
  authorizePage: AuthorizePage;
  authorizedPrincipal: Principal | undefined;
  authorizedAttributes: Record<string, string> | undefined;
}>({
  authorizeConfig: undefined,
  authorizePage: async ({ page, authorizeConfig }, use) => {
    if (authorizeConfig === undefined) {
      throw new Error("authorizeConfig must be defined");
    }

    await page.goto(authorizeConfig.testAppURL ?? "https://nice-name.com");
    const testAppPage = page;

    const internetIdentityURL =
      authorizeConfig.internetIdentityURL ?? "https://id.ai";
    const protocol = authorizeConfig.protocol ?? "legacy";

    if (protocol === "icrc25") {
      await testAppPage
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(
          internetIdentityURL +
            "/authorize" +
            ("openid" in authorizeConfig && authorizeConfig.openid !== undefined
              ? `?openid=${encodeURIComponent(authorizeConfig.openid)}`
              : ""),
        );
      await testAppPage
        .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
        .setChecked(true);
      if (
        "attributes" in authorizeConfig &&
        authorizeConfig.attributes !== undefined &&
        authorizeConfig.attributes.length > 0
      ) {
        await testAppPage
          .getByRole("textbox", { name: "Request attributes:" })
          .fill(authorizeConfig.attributes.join("\n"));
      }
    } else {
      await testAppPage
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(internetIdentityURL + "#authorize");
    }

    await expect(testAppPage.locator("#principal")).toBeHidden();
    const authPagePromise = testAppPage.context().waitForEvent("page");
    await testAppPage.getByRole("button", { name: "Sign In" }).click();
    const authPage = await authPagePromise;
    const closePromise = authPage.waitForEvent("close", { timeout: 15_000 });

    await use(new AuthorizePage(authPage));

    await closePromise;
  },
  authorizedPrincipal: async ({ page }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }

    const principal = await testAppPage.locator("#principal").textContent();
    if (principal === null) {
      return use(undefined);
    }

    await use(Principal.fromText(principal));
  },
  authorizedAttributes: async ({ page }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }

    const attributes = await testAppPage
      .locator("#certifiedAttributes")
      .innerText();
    if (attributes === "") {
      return use(undefined);
    }

    await use(
      Object.fromEntries(
        attributes.split("\n").map((line) => {
          const [key, value] = line.split(": ");
          return [key, value];
        }),
      ),
    );
  },
});
