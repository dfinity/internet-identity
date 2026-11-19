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

    test.describe("behind Apple's sheet", () => {
      test.use({ fakeAppleConfig: { sheet: true, user: appleUser } });

      test("shows Apple's connecting page in the popup and signs up when the sheet completes", async ({
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
        await fakeApple?.sheetShown;
        await expect(popup.getByText("Connecting to Apple")).toBeVisible();
        await expect(popup).toHaveURL(
          new RegExp(`^${II_URL}/apple-connecting`),
        );

        // Apple answers later than the popup takes to tell a cancel from a
        // completed sheet, so a popup that took the focus coming back for a
        // cancel would close before the sign-in goes on.
        await fakeApple?.completeSheet(popup, 1_000);
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

      test("closes the popup and returns to the sign-in methods when the sheet is canceled", async ({
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
        const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
        await fakeApple?.sheetShown;
        await expect(popup.getByText("Connecting to Apple")).toBeVisible();

        await fakeApple?.cancelSheet(popup);
        await closePromise;

        await expect(
          page.getByRole("tooltip", {
            name: "Interaction canceled. Please try again.",
          }),
        ).toBeVisible();
        await expect(appleButton).toBeEnabled();
        expect(fakeApple?.authorizeRequests).toHaveLength(1);
        await expect(page).toHaveURL(II_URL + "/");
      });
    });

    test.describe("Apple's connecting page", () => {
      for (const [label, target] of [
        ["a fragment that isn't a URL", "not a url"],
        [
          "a look-alike of Apple's origin",
          "https://appleid.apple.com.example.org/auth/authorize",
        ],
        ["another origin", "https://example.org/auth/authorize"],
      ]) {
        test(`closes without leaving II for ${label}`, async ({ page }) => {
          await page.goto(II_URL);
          const popupPromise = page.context().waitForEvent("page");
          await page.evaluate((fragment) => {
            window.open(`/apple-connecting#${fragment}`, "_blank");
          }, encodeURIComponent(target));
          const popup = await popupPromise;
          // The page sends itself on only once it has rendered, after this
          // listener is in place.
          const requestedOrigins: string[] = [];
          popup.on("request", (request) => {
            requestedOrigins.push(new URL(request.url()).origin);
          });

          await expect.poll(() => popup.isClosed()).toBe(true);
          expect(
            requestedOrigins.filter((origin) => origin !== II_URL),
          ).toEqual([]);
        });
      }
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
