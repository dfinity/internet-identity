import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import { SSO_DISCOVERY_DOMAIN, SSO_OPENID_PORT } from "../../fixtures/sso";
import { fromBase64, II_URL } from "../../utils";

// Attribute scope keys for SSO-sourced credentials are
// `sso:<domain>:<name>`, distinct from the `openid:<issuer>:<name>` form
// direct providers use. Asking for an SSO claim under the `openid:`
// form would silently miss it (PR #3805).

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

test.describe("Authorize with 1-click SSO", () => {
  test.describe("without any attributes", () => {
    const name = "John Doe";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { name }, // Claim should not be returned without explicit request
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
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
  });

  test.describe("with name and email attributes", () => {
    const name = "John Doe";
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { name, email },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
        useIcrc3Attributes: true,
        attributes: [
          `sso:${SSO_DISCOVERY_DOMAIN}:name`,
          `sso:${SSO_DISCOVERY_DOMAIN}:email`,
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
        [`sso:${SSO_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_DISCOVERY_DOMAIN}:email`]: email,
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
      // No consent screen, no manual Continue — the 1-click handler
      // certifies the auto-approve allowlist (`sso:<domain>:{name,email}`)
      // and the popup closes itself.
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
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { name },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
        useIcrc3Attributes: true,
        icrc3Nonce: knownNonce,
        attributes: [`sso:${SSO_DISCOVERY_DOMAIN}:name`],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      const map = decodeIcrc3Map(authorizedIcrc3Attributes.data);
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
    const name = "John Doe";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { name }, // No email claim — request below is unavailable
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
        useIcrc3Attributes: true,
        attributes: [
          `sso:${SSO_DISCOVERY_DOMAIN}:name`,
          `sso:${SSO_DISCOVERY_DOMAIN}:email`, // Unavailable scoped attribute
          `sso:${SSO_DISCOVERY_DOMAIN}:favorite_color`, // Unknown scoped attribute
          `favorite_food`, // Unknown unscoped attribute
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }

      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      // Only the available attribute is present.
      expect(textEntries[`sso:${SSO_DISCOVERY_DOMAIN}:name`]).toBe(name);
      expect(textEntries[`sso:${SSO_DISCOVERY_DOMAIN}:email`]).toBeUndefined();
      expect(textEntries["favorite_food"]).toBeUndefined();
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
      // the consent path instead of 1-click. The defaults (only `name`
      // available) match the assertions below — just accept.
      await consent.accept();
    });
  });

  test.describe("with verified_email attribute", () => {
    // PR #3805: the canister doesn't certify `verified_email` under
    // `sso:` yet, so `list_available_attributes` filters it out — the
    // consent path resolves to an empty set and the certified payload
    // carries no `verified_email`.
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { email, email_verified: "true" },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
        useIcrc3Attributes: true,
        attributes: [`sso:${SSO_DISCOVERY_DOMAIN}:verified_email`],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      if (authorizedIcrc3Attributes === undefined) {
        return;
      }
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(
        textEntries[`sso:${SSO_DISCOVERY_DOMAIN}:verified_email`],
      ).toBeUndefined();
    });

    test("should not return verified_email", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      // Empty groups auto-resolve to an empty consent set; no consent UI
      // interaction needed.
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("with conflicting ?openid and ?sso", () => {
    // Misconfigured sign-in URL. We open the authorize page directly
    // rather than going through `authorizePage` — that fixture ends with
    // `waitForEvent("close")`, but the channel-error view is static and
    // never closes on its own.
    test("should render ChannelError", async ({ page }) => {
      const issuer = `http://localhost:${DEFAULT_OPENID_PORT}`;
      const url = new URL(II_URL + "/authorize");
      url.searchParams.set("openid", issuer);
      url.searchParams.set("sso", SSO_DISCOVERY_DOMAIN);
      await page.goto(url.toString());
      await expect(
        page.getByRole("heading", { name: "Invalid request" }),
      ).toBeVisible();
    });
  });
});
