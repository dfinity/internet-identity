import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import {
  ALTERNATE_OPENID_PORT,
  DEFAULT_OPENID_PORT,
} from "../../fixtures/openid";
import { fromBase64, II_URL } from "../../utils";

type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: number[] }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: [string, Icrc3Value][] };

const Icrc3Value = IDL.Rec();
Icrc3Value.fill(
  IDL.Variant({
    Nat: IDL.Nat,
    Int: IDL.Int,
    Blob: IDL.Vec(IDL.Nat8),
    Text: IDL.Text,
    Array: IDL.Vec(Icrc3Value),
    Map: IDL.Vec(IDL.Tuple(IDL.Text, Icrc3Value)),
  }),
);

function decodeIcrc3Map(base64Data: string): Record<string, Icrc3Value> {
  const dataBytes = fromBase64(base64Data);
  const { Map: map } = IDL.decode([Icrc3Value], dataBytes)[0] as {
    Map: [string, Icrc3Value][];
  };
  return Object.fromEntries(map);
}

function decodeIcrc3TextEntries(base64Data: string): Record<string, string> {
  return Object.fromEntries(
    Object.entries(decodeIcrc3Map(base64Data))
      .filter(
        (entry): entry is [string, { Text: string }] => "Text" in entry[1],
      )
      .map(([key, { Text: text }]) => [key, text]),
  );
}

test.describe("Authorize with 1-click OpenID", () => {
  // Round-trip verification — see `consent.spec.ts` for rationale.
  test.afterEach(
    ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
      if (authorizedIcrc3Attributes === undefined) return;
      const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(canisterEchoedIcrc3Attributes).toEqual(expected);
    },
  );

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
        useIcrc3Attributes: true,
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeUndefined();
    });

    test("should authenticate only", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });

    // Regression guard: the 1-click flow used to sign up without recording
    // the new identity, so visiting II afterwards showed an empty identity
    // list. The identity must land in `ii-last-used-identities` on the II
    // origin, tagged `openid` so a later "last used" sign-in re-authenticates
    // through the configured-provider branch.
    test("records the new identity in last-used storage", async ({
      page,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      // Wait for the round-trip to finish (the identity is persisted before
      // the popup redirects back and the app surfaces the principal). The
      // popup-close + redirect can take a few seconds, so give it the same
      // 15s budget the rest of the authorize suite uses.
      await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });

      // Open the II origin the way a user would when they "go to id.ai" —
      // same browser context, so it shares the localStorage the popup wrote.
      const iiPage = await page.context().newPage();
      try {
        await iiPage.goto(II_URL);
        const lastUsedRaw = await iiPage.evaluate(() =>
          localStorage.getItem("ii-last-used-identities"),
        );
        expect(lastUsedRaw).not.toBeNull();
        const lastUsed = JSON.parse(lastUsedRaw!) as {
          data: Record<string, { authMethod: Record<string, unknown> }>;
        };
        // A brand-new identity was recorded (the buggy code left this empty).
        const entries = Object.values(lastUsed.data);
        expect(entries).toHaveLength(1);
        expect(entries[0]?.authMethod).toHaveProperty("openid");
        expect(entries[0]?.authMethod).not.toHaveProperty("sso");
      } finally {
        await iiPage.close();
      }
    });
  });

  // The sign-up regression tests above only exercise the sign-UP branch of
  // resumeOpenId (fresh OpenID users). This covers the sign-IN branch — a
  // returning user who signs in via 1-click on a device with no local
  // "last used" entry — which AuthFlow records through a separate code path
  // (the JWT-supplied sign-in commit in `continueWithOpenId`) that a
  // regression could break independently.
  test.describe("returning user with no local entry", () => {
    const name = "John Doe";

    test.use({
      identityConfig: { createIdentities: [{ name }] },
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { name } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
      },
    });

    // Link the OpenID credential to an existing II identity, then reset local
    // browser state so the subsequent 1-click flow reproduces a returning user
    // on a fresh device: the anchor exists on the canister (so the flow signs
    // in rather than signing up) but nothing is recorded locally yet.
    test.beforeEach(
      async ({
        page,
        identities,
        signInWithIdentity,
        signInWithOpenId,
        openIdUsers,
      }) => {
        await page.goto(II_URL + "/manage/access");
        await signInWithIdentity(page, identities[0].identityNumber);
        await page.getByRole("button", { name: "Add new" }).click();
        const popupPromise = page.context().waitForEvent("page");
        await page
          .getByRole("button", { name: openIdUsers[0].issuer.name })
          .click();
        const popup = await popupPromise;
        const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
        await signInWithOpenId(popup, openIdUsers[0].id);
        await closePromise;
        // Empty the last-used list so the assertion can only be satisfied by
        // the sign-in commit, and drop the IdP cookie so the linking session
        // isn't reused — otherwise the IdP auto-approves and skips its consent
        // screen, and `signInWithOpenId` can't find the "Authorize" heading
        // (same reason index.spec.ts clears cookies before an existing-user
        // OpenID sign-in). The (iss, sub) → anchor link lives on the canister,
        // so clearing browser state still leaves the flow a sign-IN.
        await page.evaluate(() => localStorage.clear());
        await page.context().clearCookies();
      },
    );

    test.afterEach(({ authorizedPrincipal }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    });

    test("records the signed-in identity in last-used storage", async ({
      page,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });

      const iiPage = await page.context().newPage();
      try {
        await iiPage.goto(II_URL);
        const lastUsedRaw = await iiPage.evaluate(() =>
          localStorage.getItem("ii-last-used-identities"),
        );
        expect(lastUsedRaw).not.toBeNull();
        const lastUsed = JSON.parse(lastUsedRaw!) as {
          data: Record<string, { authMethod: Record<string, unknown> }>;
        };
        // The sign-in branch recorded the identity even though localStorage
        // started empty (the buggy code left it empty).
        const entries = Object.values(lastUsed.data);
        expect(entries).toHaveLength(1);
        expect(entries[0]?.authMethod).toHaveProperty("openid");
      } finally {
        await iiPage.close();
      }
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
        useIcrc3Attributes: true,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:name`,
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      expect(authorizedIcrc3Attributes.signature.length).toBeGreaterThan(0);

      const map = decodeIcrc3Map(authorizedIcrc3Attributes.data);

      // Verify user attributes are Text
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(textEntries).toMatchObject({
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`]: name,
        [`openid:http://localhost:${DEFAULT_OPENID_PORT}:email`]: email,
      });

      // Verify implicit:origin is Text
      expect(map["implicit:origin"]).toHaveProperty("Text");

      // Verify implicit:nonce is a 32-byte Blob
      expect(map["implicit:nonce"]).toHaveProperty("Blob");
      const { Blob: nonceBlob } = map["implicit:nonce"] as {
        Blob: number[];
      };
      expect(nonceBlob).toHaveLength(32);

      // Verify implicit:issued_at_timestamp_ns is a Nat
      expect(map["implicit:issued_at_timestamp_ns"]).toHaveProperty("Nat");
      const { Nat: timestamp } = map["implicit:issued_at_timestamp_ns"] as {
        Nat: bigint;
      };
      expect(timestamp).toBeGreaterThan(BigInt(0));
    });

    test("should return attributes", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("with app-supplied nonce", () => {
    const name = "John Doe";
    // prettier-ignore
    const knownNonce = new Uint8Array([
      80, 48, 222, 48, 28, 157, 149, 134, 236, 61, 19, 71, 200, 105, 53, 187,
      44, 126, 9, 241, 76, 103, 217, 148, 12, 55, 90, 181, 33, 208, 99, 7,
    ]);

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [
          {
            claims: { name },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        icrc3Nonce: knownNonce,
        attributes: [`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      const map = decodeIcrc3Map(authorizedIcrc3Attributes.data);

      // Verify the nonce in the ICRC-3 map matches the one we supplied
      expect(map["implicit:nonce"]).toHaveProperty("Blob");
      const { Blob: nonceBlob } = map["implicit:nonce"] as {
        Blob: number[];
      };
      expect(Array.from(nonceBlob)).toEqual(Array.from(knownNonce));
    });

    test("should include the app-supplied nonce", async ({
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
        useIcrc3Attributes: true,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:name`,
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:email`, // Unavailable scoped attribute
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:favorite_color`, // Unknown scoped attribute
          `favorite_food`, // Unknown unscoped attribute
          `openid:http://localhost:${ALTERNATE_OPENID_PORT}:name`, // Wrong issuer for 1-click OpenID auto-approval
        ],
      },
    });

    // Link both OpenID provider users with identity first to ensure attributes from
    // both providers are available, but only the active issuer's are auto-approved.
    test.beforeEach(
      async ({
        page,
        identities,
        signInWithIdentity,
        signInWithOpenId,
        openIdUsers,
      }) => {
        await page.goto(II_URL + "/manage/access");
        await signInWithIdentity(page, identities[0].identityNumber);

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

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      const blobEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      // Only the name from the default (active) provider — auto-approved
      // via 1-click OpenID — should be present.
      expect(
        blobEntries[`openid:http://localhost:${DEFAULT_OPENID_PORT}:name`],
      ).toBe(defaultName);
      expect(
        blobEntries[`openid:http://localhost:${DEFAULT_OPENID_PORT}:email`],
      ).toBeUndefined();
      expect(blobEntries["favorite_food"]).toBeUndefined();
      expect(
        blobEntries[`openid:http://localhost:${ALTERNATE_OPENID_PORT}:name`],
      ).toBeUndefined();
    });

    test("should omit attributes", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      // Mixing 1-click-eligible keys with non-eligible / unknown ones takes
      // the consent path instead of 1-click. The two scoped `name`
      // requests render as separate rows (one per requested issuer); the
      // user has to explicitly uncheck the alternate-issuer row to keep
      // it out of the certified payload, which is what the assertions
      // below verify.
      await consent.waitForVisible();
      await consent.uncheckRow(`Test OpenID ${ALTERNATE_OPENID_PORT} name:`);
      await consent.continue();
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
        useIcrc3Attributes: true,
        attributes: [
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:verified_email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      const blobEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(
        blobEntries[
          `openid:http://localhost:${DEFAULT_OPENID_PORT}:verified_email`
        ],
      ).toBe(email);
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
