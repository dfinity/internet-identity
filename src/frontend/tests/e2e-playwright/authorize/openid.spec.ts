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
  { title: "name only", openIdClaims: { name: "John Doe" } },
  { title: "email only", openIdClaims: { email: "john.doe@example.com" } },
  {
    title: "name and email",
    openIdClaims: { name: "John Doe", email: "john.doe@example.com" },
  },
  {
    title: "name, email, and extra (not returned)",
    openIdClaims: {
      name: "John Doe",
      email: "john.doe@example.com",
      favoriteColor: "blue",
    },
  },
];

const returnedClaimKeys = new Set(["name", "email"]);

test.describe(
  "Authorize with ICRC-29 (directly through OpenID) and request attributes",
  () => {
    openIdClaimCases.forEach(({ title, openIdClaims }) => {
      test.use({ openIdClaims });

      test(title, async ({ page, openIdIssuer, openIdUser }) => {
        await authorizeWithUrl(
          page,
          TEST_APP_URL,
          II_URL + `/authorize?openid=${encodeURIComponent(openIdIssuer)}`,
          (openIdPage) => openIdUser.signIn(openIdPage),
          true,
        );
        expect(page.locator("#certifiedAttributes")).toHaveText(
          Object.entries(openIdUser.claims)
            .filter(([key]) => returnedClaimKeys.has(key))
            .sort(([keyA], [keyB]) => keyA.localeCompare(keyB))
            .map(([key, value]) => `openid:${openIdIssuer}:${key}: ${value}`)
            .join("\n"),
        );
      });
    });
  },
);
