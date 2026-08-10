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
  // Every redirect flow must reach the RP session key through browser-signed
  // hops, never as the key the canister certified directly: what the canister
  // certifies transits the IC and must be inert on its own.
  test.afterEach(({ authorizedDelegation }) => {
    expect(authorizedDelegation).toBeDefined();
    if (authorizedDelegation === undefined) {
      return;
    }
    // The chain runs root (user identity) → app-delegation key → intermediate
    // key → RP session key. Three hops on this transport: the delegation
    // handler certifies to a key Internet Identity keeps (so a later
    // `?prompt=none` can re-issue without a ceremony) and extends it, and the
    // redirect transport adds its own per-flow intermediate key on top.
    const { delegations } = authorizedDelegation;
    expect(delegations).toHaveLength(3);
    // The canister-certified hop (delegations[0]) must not target the RP session
    // key, which is only ever the chain's leaf.
    expect(delegations[0].delegation.pubkey).not.toBe(
      delegations[delegations.length - 1].delegation.pubkey,
    );
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
