import { expect } from "@playwright/test";
import { test } from "../fixtures";
import { ALTERNATE_OPENID_PORT, DEFAULT_OPENID_PORT } from "../fixtures/openid";
import { II_URL } from "../utils";

test.describe("Authorize with direct OpenID", () => {
  test.describe("without any attributes", () => {
    const name = "John Doe";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [
          {
            claims: { name }, // Claim should not be returned without explicit request
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedAttributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedAttributes).toBeUndefined();
    });

    test("should authenticate only", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("with name and email attributes", () => {
    const name = "John Doe";
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [
          {
            claims: { name, email },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:name`,
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedAttributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedAttributes).toEqual({
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`]: name,
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:email`]: email,
      });
    });

    test("should return attributes", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("with unavailable attribute", () => {
    const defaultName = "John Doe";
    const alternateName = "Jane Doe";

    test.use({
      openIdConfig: {
        createUsers: [
          {
            port: DEFAULT_OPENID_PORT,
            claims: { name: defaultName },
          },
          {
            port: ALTERNATE_OPENID_PORT,
            claims: { name: alternateName },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:name`,
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:email`, // Unavailable scoped attribute
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:favorite_color`, // Unknown scoped attribute
          `favorite_food`, // Unknown unscoped attribute
          `openid:http://localhost:${ALTERNATE_OPENID_PORT}:name`, // Missing implicit consent
        ],
      },
    });

    // Link both OpenID provider users with identity first to ensure attributes from
    // both providers are available, but only one is returned through implicit consent.
    test.beforeEach(
      async ({ page, identity, signInWithOpenId, openIdUsers }) => {
        await page.goto(II_URL + "/manage/access");
        await identity.signIn();

        for (const user of openIdUsers) {
          // Click "Add new" and pick OpenID provider
          await page.getByRole("button", { name: "Add new" }).click();
          const popupPromise = page.context().waitForEvent("page");
          await page.getByRole("button", { name: user.issuer.name }).click();

          // Sign in on OpenID page
          const popup = await popupPromise;
          const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
          await signInWithOpenId(popup, user.id);
          await closePromise;
        }
      },
    );

    test.afterEach(({ authorizedPrincipal, authorizedAttributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedAttributes).toEqual({
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`]: defaultName,
      });
    });

    test("should omit attributes", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("with verified_email attribute", () => {
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [
          {
            claims: { email, email_verified: "true" },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:verified_email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedAttributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedAttributes).toEqual({
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:verified_email`]:
          email,
      });
    });

    test("should return verified email", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });
});
