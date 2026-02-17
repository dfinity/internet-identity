import { expect } from "@playwright/test";
import { test } from "../fixtures";
import { authorizeWithUrl, TEST_APP_URL, II_URL } from "../utils";

test("Authorize with ICRC-29 (directly through OpenID)", async ({
  openIdIssuer,
  openIdUser,
  page,
}) => {
  await authorizeWithUrl(
    page,
    TEST_APP_URL,
    II_URL + `/authorize?openid=${encodeURIComponent(openIdIssuer)}`,
    (openIdPage) => openIdUser.signIn(openIdPage),
    true,
  );
});

const openIdClaimCases = [
  {
    title: "name and email",
    openIdClaims: { name: "John Doe", email: "john.doe@example.com" },
  },
];

test.describe("Authorize with ICRC-29 (directly through OpenID) and request attributes", () => {
  openIdClaimCases.forEach(({ title, openIdClaims }) => {
    test.use({ openIdClaims });

    // TODO: Fix these tests, they currently fail due to https requirements.
    test(title, async ({ page, openIdIssuer, openIdUser }) => {
      await authorizeWithUrl(
        page,
        TEST_APP_URL,
        II_URL + `/authorize?openid=${encodeURIComponent(openIdIssuer)}`,
        (openIdPage) => openIdUser.signIn(openIdPage),
        true,
        Object.keys(openIdClaims).map((key) => `openid:${openIdIssuer}:${key}`),
      );
      await expect(page.locator("#certifiedAttributes")).toHaveText(
        Object.entries(openIdUser.claims)
          .sort(([keyA], [keyB]) => keyA.localeCompare(keyB))
          .map(([key, value]) => `openid:${openIdIssuer}:${key}: ${value}`)
          .join(""),
      );
    });
  });
});
