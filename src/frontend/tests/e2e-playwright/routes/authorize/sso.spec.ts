import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import { SSO_DISCOVERY_DOMAIN, SSO_OPENID_PORT } from "../../fixtures/sso";
import { fromBase64 } from "../../utils";

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

// SSO is just OpenID with a couple of extra wizard steps: click
// "Continue with SSO", type the company domain, wait for the two-hop
// discovery to resolve, then the IdP popup that opens is the same
// `test_openid_provider` page direct-OpenID tests drive. The
// 1-click-OpenID auto-approval path covered in `openid.spec.ts` is
// intentionally not duplicated here.
//
// Attribute scope keys use the SSO form `sso:<domain>:<name>` rather
// than the `openid:<issuer>:<name>` form direct providers use — see
// PR #3805. SSO-sourced credentials are addressable via `sso:` only,
// `openid:` would silently miss them.
test.describe("Authorize via SSO", () => {
  test.describe("without any attributes", () => {
    const name = "John Doe";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            // Claim should not be returned without explicit request,
            // matching the analogous direct-OpenID scenario.
            claims: { name },
          },
        ],
      },
      // No `openid` hint here: that would route to the direct-OpenID
      // flow for `Test OpenID 11107`. The SSO tests need the user to
      // land on the full picker so they can click "Continue with SSO".
      authorizeConfig: {
        protocol: "icrc25",
        useIcrc3Attributes: true,
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeUndefined();
    });

    test("should authenticate only", async ({
      authorizePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const ssoPage = await openSsoPopup(authorizePage.page);
      const closePromise = ssoPage.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(ssoPage, openIdUsers[0].id);
      await closePromise;
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
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
        useIcrc3Attributes: true,
        // SSO attributes use `sso:<domain>:<name>`. The `<domain>` is
        // the discovery domain registered via
        // `add_discoverable_oidc_config` (matches the credential's
        // recorded discovery_domain), not the IdP's issuer URL.
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
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(textEntries).toMatchObject({
        [`sso:${SSO_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_DISCOVERY_DOMAIN}:email`]: email,
      });

      expect(map["implicit:origin"]).toHaveProperty("Text");
      expect(map["implicit:nonce"]).toHaveProperty("Blob");
      const { Blob: nonceBlob } = map["implicit:nonce"] as {
        Blob: number[];
      };
      expect(nonceBlob).toHaveLength(32);
      expect(map["implicit:issued_at_timestamp_ns"]).toHaveProperty("Nat");
    });

    test("should return attributes", async ({
      authorizePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const ssoPage = await openSsoPopup(authorizePage.page);
      const closePromise = ssoPage.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(ssoPage, openIdUsers[0].id);
      await closePromise;
      // 1-click is intentionally not exercised here. The user has no
      // pre-existing credential for this issuer, so the wizard takes
      // the consent path with all defaults selected.
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  // 1-click SSO: the dapp passes `?sso=<domain>` on the authorize URL,
  // II runs discovery + redirect immediately without showing the auth
  // wizard. Equivalent of the `?openid=<issuer>` 1-click that direct
  // OpenID providers already use.
  test.describe("1-click via ?sso= search param", () => {
    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [
          {
            claims: { name: "John Doe" },
          },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        sso: SSO_DISCOVERY_DOMAIN,
        useIcrc3Attributes: true,
      },
    });

    test.afterEach(({ authorizedPrincipal }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    });

    test("redirects straight to the IdP without showing the wizard", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      // The /authorize page in this case does NOT render the wizard —
      // it kicks off the SSO redirect on mount. We end up on the IdP
      // sign-in form directly, then come back to the auth page for the
      // final Continue.
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  // Both `?openid=` and `?sso=` set in one URL is a misconfigured
  // sign-in URL. Rather than silently picking one entry point, II
  // surfaces it as an "invalid request" via the existing ChannelError
  // UI so the dapp gets a clear signal to fix the URL.
  test.describe("conflicting ?openid and ?sso", () => {
    test.use({
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        sso: SSO_DISCOVERY_DOMAIN,
      },
    });

    test("renders the channel error", async ({ authorizePage }) => {
      await expect(
        authorizePage.page.getByRole("heading", { name: "Invalid request" }),
      ).toBeVisible();
    });
  });
});
