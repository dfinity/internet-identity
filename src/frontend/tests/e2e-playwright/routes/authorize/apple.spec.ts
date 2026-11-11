import { expect } from "@playwright/test";
import { test } from "../../fixtures";
import {
  APPLE_ISSUER,
  APPLE_OPENID_PORT,
  SAFARI_USER_AGENT,
} from "../../fixtures/apple";
import { II_URL } from "../../utils";

const email = "jane@privaterelay.appleid.com";

test.describe("1-click Sign in with Apple in Safari", () => {
  test.use({
    userAgent: SAFARI_USER_AGENT,
    // Apple keeps the user's name out of its ID tokens.
    openIdConfig: {
      defaultPort: APPLE_OPENID_PORT,
      createUsers: [{ claims: { email } }],
    },
    authorizeConfig: { protocol: "icrc25", openid: APPLE_ISSUER },
    fakeAppleConfig: {
      sheet: true,
      user: { name: { firstName: "Jane", lastName: "Appleseed" }, email },
    },
  });

  test("closes the window when Apple's sheet is canceled", async ({
    page,
    authorizePage,
    fakeApple,
  }) => {
    const iiPage = authorizePage.page;
    const closePromise = iiPage.waitForEvent("close", { timeout: 15_000 });
    await fakeApple?.sheetShown;
    await expect(iiPage.getByText("Connecting to Apple")).toBeVisible();

    await fakeApple?.cancelSheet(iiPage);

    await closePromise;
    expect(fakeApple?.authorizeRequests).toHaveLength(1);
    await expect(page.locator("#principal")).toBeHidden();
  });

  test("signs in when Apple's sheet completes, named by Apple", async ({
    page,
    authorizePage,
    fakeApple,
    signInWithOpenId,
    openIdUsers,
  }) => {
    const iiPage = authorizePage.page;
    await fakeApple?.sheetShown;
    await expect(iiPage.getByText("Connecting to Apple")).toBeVisible();

    // Apple answers later than the window takes to tell a cancel from a
    // completed sheet, so a page that took the focus coming back for a cancel
    // would close before the sign-in goes on.
    await fakeApple?.completeSheet(iiPage, 1_000);
    await signInWithOpenId(iiPage, openIdUsers[0].id);

    await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });
    expect(
      fakeApple?.authorizeRequests.map((request) =>
        request.searchParams.get("response_mode"),
      ),
    ).toEqual(["form_post"]);

    // The identity was signed up with the name Apple sent to /callback.
    const lastUsedPage = await page.context().newPage();
    try {
      await lastUsedPage.goto(II_URL);
      const lastUsedRaw = await lastUsedPage.evaluate(() =>
        localStorage.getItem("ii-last-used-identities"),
      );
      expect(lastUsedRaw).not.toBeNull();
      const lastUsed = JSON.parse(lastUsedRaw!) as {
        data: Record<string, { name?: string }>;
      };
      expect(Object.values(lastUsed.data).map(({ name }) => name)).toEqual([
        "Jane Appleseed",
      ]);
    } finally {
      await lastUsedPage.close();
    }
  });
});
