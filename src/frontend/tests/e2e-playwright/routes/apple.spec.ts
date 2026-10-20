import { expect } from "@playwright/test";
import { II_URL } from "../utils";
import { test } from "../fixtures";
import {
  APPLE_OPENID_PORT,
  APPLE_PROVIDER_NAME,
  SAFARI_USER_AGENT,
} from "../fixtures/apple";

const email = "jane@privaterelay.appleid.com";
const appleUser = {
  name: { firstName: "Jane", lastName: "Appleseed" },
  email,
};
const appleName = "Jane Appleseed";

test.describe("Sign in with Apple", () => {
  // Apple keeps the user's name out of its ID tokens.
  test.use({
    openIdConfig: {
      defaultPort: APPLE_OPENID_PORT,
      createUsers: [{ claims: { email } }],
    },
  });

  test.describe("in Safari", () => {
    test.use({ userAgent: SAFARI_USER_AGENT });

    test.describe("signing in", () => {
      test.use({ fakeAppleConfig: { user: appleUser } });

      test("signs up through Apple's web_message reply, named by Apple", async ({
        page,
        fakeApple,
        signInWithOpenId,
        openIdUsers,
      }) => {
        await page.goto(II_URL);
        const popupPromise = page.context().waitForEvent("page");
        await page.getByRole("button", { name: APPLE_PROVIDER_NAME }).click();
        const popup = await popupPromise;
        const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
        await signInWithOpenId(popup, openIdUsers[0].id);
        await closePromise;

        expect(
          fakeApple?.authorizeRequests.map((request) =>
            request.searchParams.get("response_mode"),
          ),
        ).toEqual(["web_message"]);

        await page
          .getByRole("dialog")
          .getByRole("button", { name: "Sign up" })
          .click();
        await page.waitForURL(II_URL + "/manage");
        await expect(
          page.getByRole("heading", {
            name: new RegExp(`Welcome, ${appleName}\\.`),
          }),
        ).toBeVisible();
      });
    });

    test.describe("canceling", () => {
      test.use({ fakeAppleConfig: { cancel: true } });

      test("returns to the sign-in methods when Apple's sheet is canceled", async ({
        page,
        fakeApple,
      }) => {
        await page.goto(II_URL);
        const popupPromise = page.context().waitForEvent("page");
        const appleButton = page.getByRole("button", {
          name: APPLE_PROVIDER_NAME,
        });
        await appleButton.click();
        const popup = await popupPromise;
        // Apple answers at once, so the popup can be closed before a listener
        // for its close event would be in place.
        await expect.poll(() => popup.isClosed()).toBe(true);

        expect(
          fakeApple?.authorizeRequests.map((request) =>
            request.searchParams.get("response_mode"),
          ),
        ).toEqual(["web_message"]);
        // The picker's cancel hint, which an error from Apple never shows.
        await expect(
          page.getByRole("tooltip", {
            name: "Interaction canceled. Please try again.",
          }),
        ).toBeVisible();
        await expect(appleButton).toBeEnabled();
        await expect(page).toHaveURL(II_URL + "/");
      });
    });

    test.describe("canceling without a reply", () => {
      test.use({ fakeAppleConfig: { silentCancel: true } });

      test("returns to the sign-in methods when Apple's sheet closes without a reply", async ({
        page,
        fakeApple,
      }) => {
        await page.goto(II_URL);
        const popupPromise = page.context().waitForEvent("page");
        const appleButton = page.getByRole("button", {
          name: APPLE_PROVIDER_NAME,
        });
        await appleButton.click();
        const popup = await popupPromise;
        await expect.poll(() => fakeApple?.authorizeRequests.length).toBe(1);

        // The sheet goes away: the page gets its focus back, and nothing
        // arrives from Apple.
        await page.evaluate(() => {
          window.dispatchEvent(new FocusEvent("blur"));
          window.dispatchEvent(new FocusEvent("focus"));
        });

        await expect(
          page.getByRole("tooltip", {
            name: "Interaction canceled. Please try again.",
          }),
        ).toBeVisible();
        await expect(appleButton).toBeEnabled();
        await expect.poll(() => popup.isClosed()).toBe(true);
        expect(
          fakeApple?.authorizeRequests.map((request) =>
            request.searchParams.get("response_mode"),
          ),
        ).toEqual(["web_message"]);
        await expect(page).toHaveURL(II_URL + "/");
      });
    });
  });

  test.describe("outside Safari", () => {
    test.use({ fakeAppleConfig: { user: appleUser } });

    test("signs up through /callback, named by Apple's user field", async ({
      page,
      fakeApple,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await page.goto(II_URL);
      const popupPromise = page.context().waitForEvent("page");
      await page.getByRole("button", { name: APPLE_PROVIDER_NAME }).click();
      const popup = await popupPromise;
      const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(popup, openIdUsers[0].id);
      await closePromise;

      expect(
        fakeApple?.authorizeRequests.map((request) =>
          request.searchParams.get("response_mode"),
        ),
      ).toEqual(["form_post"]);

      await page
        .getByRole("dialog")
        .getByRole("button", { name: "Sign up" })
        .click();
      await page.waitForURL(II_URL + "/manage");
      await expect(
        page.getByRole("heading", {
          name: new RegExp(`Welcome, ${appleName}\\.`),
        }),
      ).toBeVisible();
    });
  });
});
