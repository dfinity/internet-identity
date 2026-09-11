import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import { SSO_DISCOVERY_DOMAIN, SSO_OPENID_PORT } from "../../fixtures/sso";
import { addVirtualAuthenticator, fromBase64 } from "../../utils";

// Authorize over the ICRC-167 URL (redirect) transport — the same flows the
// window/postMessage specs cover, driven single-tab through the test app's
// `/callback`. Selected via `authorizeConfig.transport: "redirect"`.

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

const decodeIcrc3TextEntries = (base64Data: string): Record<string, string> => {
  const { Map: map } = IDL.decode([Icrc3Value], fromBase64(base64Data))[0] as {
    Map: [string, { Text?: string }][];
  };
  return Object.fromEntries(
    map
      .filter(
        (entry): entry is [string, { Text: string }] => "Text" in entry[1],
      )
      .map(([key, { Text: text }]) => [key, text]),
  );
};

test.describe("Authorize over the redirect transport", () => {
  // The app asks for a session here, not for a delegation, so what it signs
  // with is the app delegation chain it then mints for itself: one hop, signed
  // by the canister to the app's own key and rooted at the account's key. The
  // session chain those mints are made as is the two-hop one, and the app never
  // signs with it.
  //
  // A delegation asked for over this transport — `icrc34_delegation` — is the
  // chain that gets the intermediate-key treatment instead, because it travels
  // back through the URL and what the canister certifies must be inert on its
  // own. Nothing certified rides the URL in a session flow: the app mints after
  // the return. `transport/url.test.ts` covers that chain's two hops.
  test.afterEach(({ authorizedDelegation }) => {
    expect(authorizedDelegation).toBeDefined();
    if (authorizedDelegation === undefined) {
      return;
    }
    const { delegations, publicKey } = authorizedDelegation;
    expect(delegations).toHaveLength(1);
    // Rooted at the account's key and reaching the app's own: a hop whose
    // target is the root would be the chain going nowhere.
    expect(delegations[0].delegation.pubkey).not.toBe(publicKey);
    // Minutes, not days. `APP_DELEGATION_TTL_NS` is five, and the bound is what
    // makes a canister-signed hop straight to the app's key safe to hand over:
    // the 30-day outer delegation of a `icrc34_delegation` chain would fail
    // here. Loose enough either way for the replica's clock to sit a little
    // apart from the browser's.
    const expiresAt = Number(
      BigInt(`0x${delegations[0].delegation.expiration}`) / BigInt(1_000_000),
    );
    expect(expiresAt).toBeGreaterThan(Date.now() - 2 * 60_000);
    expect(expiresAt).toBeLessThan(Date.now() + 10 * 60_000);
  });

  test.describe("passkey sign-up", () => {
    test.use({
      authorizeConfig: { protocol: "icrc25", transport: "redirect" },
    });

    test.afterEach(({ authorizedPrincipal }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    });

    test("creates a new identity and authenticates", async ({
      authorizePage,
    }) => {
      const page = authorizePage.page;
      await addVirtualAuthenticator(page);

      const continueWithPasskey = page.getByRole("button", {
        name: "Continue with passkey",
      });
      const createToggle = page.getByRole("button", {
        name: "Create",
        exact: true,
      });
      await continueWithPasskey.or(createToggle).first().waitFor();
      if (await continueWithPasskey.isVisible()) {
        await continueWithPasskey.click();
        await page.getByRole("button", { name: "Create new identity" }).click();
      } else {
        await createToggle.click();
        await page.getByRole("button", { name: "Create with passkey" }).click();
      }
      await page.getByLabel("Identity name").fill("Test User");
      await page.getByRole("button", { name: "Create identity" }).click();

      // The redirect flow surfaces II's authorize screen; continue through it
      // to deliver the delegation back to the test app.
      await page.getByRole("button", { name: "Continue", exact: true }).click();
    });
  });

  test.describe("1-click OpenID", () => {
    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { name: "John Doe" } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        transport: "redirect",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
      },
    });

    test.afterEach(({ authorizedPrincipal }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    });

    // Exercises the full redirect round-trip, including II's own top-level
    // redirect to the OpenID provider and back (the transport resumes its
    // flow from sessionStorage across that hop).
    test("authenticates", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });

  test.describe("1-click SSO with attributes", () => {
    const name = "John Doe";
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        transport: "redirect",
        sso: SSO_DISCOVERY_DOMAIN,
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
      expect(
        decodeIcrc3TextEntries(authorizedIcrc3Attributes.data),
      ).toMatchObject({
        [`sso:${SSO_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_DISCOVERY_DOMAIN}:email`]: email,
      });
    });

    // The signed attributes are delivered back to the test app over the same
    // redirect as the delegation (batched into one response). Like the 1-click
    // OpenID flow, sign-in auto-returns — no "Continue" step to click through.
    test("returns the requested attributes", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    });
  });
});
