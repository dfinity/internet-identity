import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import {
  SSO_DISCOVERY_DOMAIN,
  SSO_GATING_DISCOVERY_DOMAIN,
  SSO_OPENID_PORT,
  SSO_PER_APP_CLIENT_ID,
} from "../../fixtures/sso";
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

    // Regression guard: the 1-click flow used to sign up without recording
    // the new identity. It must land in `ii-last-used-identities` on the II
    // origin, and — because the resume flow redeems the SSO JWT through
    // `continueWithOpenId` — it must be tagged `sso` (keyed by domain), not
    // `openid`. An `openid` entry would break a later "last used" sign-in,
    // whose SSO branch needs the discovery domain to re-run discovery.
    test("records the new identity as an sso entry in last-used storage", async ({
      page,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      // The popup-close + redirect can take a few seconds; use the same 15s
      // budget the rest of the authorize suite uses.
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
        // A brand-new identity was recorded (the buggy code left this empty).
        const entries = Object.values(lastUsed.data);
        expect(entries).toHaveLength(1);
        expect(entries[0]?.authMethod).toHaveProperty("sso");
        expect(entries[0]?.authMethod).not.toHaveProperty("openid");
        const sso = (entries[0]!.authMethod as { sso: { domain: string } }).sso;
        expect(sso.domain).toBe(SSO_DISCOVERY_DOMAIN);
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

// ===========================================================================
// IdP-side per-app SSO gating (docs/ongoing/enterprise-sso-idp-side-gating.md)
//
// The test provider serves `app_clients` / `gate_all_apps` in its well-known
// (configured via `configureSsoGating`), so a dapp origin becomes gated (served
// by a dedicated per-app client) or denied. The frontend threads the dapp
// origin into `get_sso_discovery` (→ `resolved_client_id`) and signs in via
// `sso_prepare_delegation` (the SSO gate path).
//
// Both cases discover through SSO_GATING_DISCOVERY_DOMAIN (`127.0.0.1:11107`),
// NOT the un-gated tests' `localhost:11107`: II caches discovery per domain for
// ~1h with no force-refresh, so a shared domain would let an earlier un-gated
// lookup pin a stale config here (and vice-versa). A distinct host string is a
// distinct cache key, isolating the two suites in any shard order.
//
// To keep that single gating cache entry coherent no matter which test warms it
// first, BOTH cases use ONE gating config: the gated origin is listed (served by
// the per-app client) and `gate_all_apps` denies everything else. So the gated
// test uses the listed origin and the denied test uses an unlisted one — nothing
// mutates the config between them.
// ===========================================================================
test.describe("Authorize with IdP-side per-app gating", () => {
  const name = "John Doe";
  // Listed in `app_clients` → served by the per-app client. The frontend remaps
  // `testAppURL` to the effective origin used for the delegation/gate; for
  // `nice-name.com` that is the origin itself.
  const GATED_ORIGIN = "https://nice-name.com";
  // Absent from `app_clients` → denied by `gate_all_apps`.
  const DENIED_ORIGIN = "https://denied-app.com";

  // One config for both tests (see the block comment): the gated origin is
  // allowed via its per-app client, every other origin is denied.
  const gatingConfig = {
    appClients: { [GATED_ORIGIN]: SSO_PER_APP_CLIENT_ID },
    gateAllApps: true,
  };

  test.describe("gated origin (assigned user)", () => {
    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        testAppURL: GATED_ORIGIN,
        useIcrc3Attributes: true,
      },
    });

    test.afterEach(({ authorizedPrincipal }) => {
      // Reaching a non-anonymous principal proves the whole gated chain
      // (per-app `aud` accepted, identity registered, gated session minted,
      // delegation issued to the dapp) completed.
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    });

    test("first gated login registers directly, then reaches the app", async ({
      authorizePage,
      signInWithOpenId,
      openIdUsers,
      configureSsoGating,
    }) => {
      // A brand-new user hitting a gated app whose org keys identities on the
      // stable `sub` registers DIRECTLY through the per-app client in a single
      // IdP trip (§6.1) — no "sign in normally first" detour. Auto-drive the one
      // IdP popup the ceremony opens with the assigned user, then finalise on the
      // standard post-registration "Continue to <app>" account screen.
      await configureSsoGating(gatingConfig);
      authorizePage.page.context().on("page", (popup) => {
        void signInWithOpenId(popup, openIdUsers[0].id).catch(() => {
          // A popup can close before it's driven; ignore and let the flow
          // proceed.
        });
      });
      await authorizePage.page
        .getByRole("button", { name: "Sign in with SSO" })
        .click();
      await authorizePage.page
        .getByRole("textbox", { name: "Company domain" })
        .fill(SSO_GATING_DISCOVERY_DOMAIN);
      const domainContinue = authorizePage.page.getByRole("button", {
        name: "Continue",
        exact: true,
      });
      await expect(domainContinue).toBeEnabled({ timeout: 30_000 });
      await domainContinue.click();
      // Direct registration lands on the account screen; continue to the dapp.
      await expect(
        authorizePage.page.getByRole("heading", {
          name: "Continue to Test Dapp",
        }),
      ).toBeVisible({ timeout: 30_000 });
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    });
  });

  test.describe("denied origin (gate_all_apps)", () => {
    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        testAppURL: DENIED_ORIGIN,
        useIcrc3Attributes: true,
      },
    });

    test("gate_all_apps blocks an unlisted dapp", async ({
      authorizePage,
      configureSsoGating,
    }) => {
      // With `gate_all_apps` on and this origin absent from `app_clients`,
      // discovery resolves the domain but denies the origin, so the Continue
      // button never enables and an inline "not granted" error is shown — no
      // delegation is issued.
      await configureSsoGating(gatingConfig);
      await authorizePage.page
        .getByRole("button", { name: "Sign in with SSO" })
        .click();
      await authorizePage.page
        .getByRole("textbox", { name: "Company domain" })
        .fill(SSO_GATING_DISCOVERY_DOMAIN);
      await expect(
        authorizePage.page.getByText(
          /hasn't granted this app access|not granted/i,
        ),
      ).toBeVisible({ timeout: 30_000 });
      // This flow never authorizes, so the auth tab won't close on its own;
      // close it so the `authorizePage` fixture's teardown (which awaits
      // `close`) doesn't time out. Mirrors the conflicting-`?openid`/`?sso` test.
      await authorizePage.page.close();
    });
  });
});

// ===========================================================================
// Manual "Sign in with SSO" parity (regression guard for the wizard path)
//
// The 1-click `?sso=` entry resolves the dapp origin (`waitForEffectiveOrigin`)
// and signs in via `authenticateWithSso`, attaching the certified attribute
// bundle. The MANUAL wizard entry ("Sign in with SSO" → type the company
// domain) must do the same: it takes `ssoOrigin` (the effective origin,
// forwarded through `AuthWizardView`) so `continueWithSso` builds an `sso`
// context and `authenticateWithSso` attaches the bundle. If that origin were
// dropped, the wizard would fall back to the plain OpenID path with NO bundle,
// so `read_certified_sso_bundle` would return `None` and the session would
// list/certify NO `sso:<domain>` attributes. This asserts the manual path both
// LISTS the SSO rows (consent) and CERTIFIES/RETURNS them (round-tripped to the
// dapp), mirroring the 1-click attribute test.
// ===========================================================================
test.describe("Authorize with manual Sign in with SSO", () => {
  const name = "John Doe";
  const email = "john.doe@example.com";

  // Round-trip verification — the certified bundle the dapp receives must
  // replay byte-for-byte through the test_app canister (see `consent.spec.ts`).
  test.afterEach(
    ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
      if (authorizedIcrc3Attributes === undefined) return;
      const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(canisterEchoedIcrc3Attributes).toEqual(expected);
    },
  );

  test.use({
    openIdConfig: {
      defaultPort: SSO_OPENID_PORT,
      createUsers: [{ claims: { name, email } }],
    },
    // No `sso:` field → the regular authorize entry (the wizard), NOT the
    // `?sso=` 1-click path. The dapp still requests the SSO-scoped attributes.
    authorizeConfig: {
      protocol: "icrc25",
      useIcrc3Attributes: true,
      attributes: [
        `sso:${SSO_DISCOVERY_DOMAIN}:name`,
        `sso:${SSO_DISCOVERY_DOMAIN}:email`,
      ],
    },
  });

  test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
    expect(authorizedPrincipal?.isAnonymous()).toBe(false);
    // Certified/returned: the manual session attached the bundle, so the dapp
    // received the SSO attributes.
    expect(authorizedIcrc3Attributes).toBeDefined();
    if (authorizedIcrc3Attributes === undefined) return;
    const textEntries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
    expect(textEntries).toMatchObject({
      [`sso:${SSO_DISCOVERY_DOMAIN}:name`]: name,
      [`sso:${SSO_DISCOVERY_DOMAIN}:email`]: email,
    });
  });

  test("lists and certifies SSO attributes via the manual wizard entry", async ({
    attributeConsentView,
    authorizePage,
    openSsoPopup,
    signInWithOpenId,
    openIdUsers,
  }) => {
    const consent = attributeConsentView(authorizePage.page);
    // Manual entry: click "Sign in with SSO" and type the company domain.
    const ssoPage = await openSsoPopup(
      authorizePage.page,
      SSO_DISCOVERY_DOMAIN,
      "signin",
    );
    const closePromise = ssoPage.waitForEvent("close", { timeout: 15_000 });
    await signInWithOpenId(ssoPage, openIdUsers[0].id);
    await closePromise;
    // Fresh SSO user → IdentityNotConnectedDialog; confirm sign-up.
    await authorizePage.page
      .getByRole("dialog")
      .getByRole("button", { name: "Sign up" })
      .click();
    await authorizePage.page
      .getByRole("button", { name: "Continue", exact: true })
      .click();
    // LISTED: the consent screen surfaces the SSO-scoped rows (only possible
    // when `list_available_attributes` saw the certified bundle).
    await consent.waitForVisible();
    await expect(
      consent.row(`Test SSO ${SSO_OPENID_PORT} email:`),
    ).toBeVisible();
    await expect(
      consent.row(`Test SSO ${SSO_OPENID_PORT} name:`),
    ).toBeVisible();
    await consent.continue();
  });
});

// ===========================================================================
// "Continue as a last-used SSO identity" parity (the real coverage hole)
//
// After an SSO sign-in stores a last-used SSO entry, a later authorize load
// with no active session shows the ContinueView "Continue" button. That
// re-auth path (`authLastUsedFlow.authenticate`) must ALSO redeem through the
// gate path (`authenticateWithSso`) so the certified attribute bundle is
// attached — otherwise it falls back to the plain OpenID path with no bundle,
// and the session lists/certifies NO `sso:<domain>` attributes (the exact bug
// the user hit). This drives two authorize ceremonies in one context: a
// 1-click SSO sign-in to seed the last-used entry, then a regular authorize
// where we click "Continue" and assert the SSO attributes are BOTH listed
// (consent rows) AND certified/returned (round-tripped to the dapp).
// ===========================================================================
test.describe("Continue as a last-used SSO identity", () => {
  const name = "John Doe";
  const email = "john.doe@example.com";

  // Round-trip verification: the certified bundle the dapp receives on the
  // SECOND (Continue) authorize must replay through the test_app canister.
  test.afterEach(
    ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
      if (authorizedIcrc3Attributes === undefined) return;
      const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(canisterEchoedIcrc3Attributes).toEqual(expected);
    },
  );

  test.use({
    openIdConfig: {
      defaultPort: SSO_OPENID_PORT,
      createUsers: [{ claims: { name, email } }],
    },
    // The FIRST authorize is `?sso=` 1-click, which seeds the last-used SSO
    // entry. The SECOND authorize (driven in the body) drops `?sso=` so the
    // ContinueView path runs.
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
    // Certified/returned on the Continue re-auth: the bundle was attached.
    expect(authorizedIcrc3Attributes).toBeDefined();
    if (authorizedIcrc3Attributes === undefined) return;
    const textEntries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
    expect(textEntries).toMatchObject({
      [`sso:${SSO_DISCOVERY_DOMAIN}:name`]: name,
      [`sso:${SSO_DISCOVERY_DOMAIN}:email`]: email,
    });
  });

  test("Continue re-auth attaches the bundle → lists + certifies SSO attributes", async ({
    page,
    attributeConsentView,
    authorizePage,
    signInWithOpenId,
    openIdUsers,
  }) => {
    // FIRST authorize: 1-click SSO seeds the last-used SSO identity.
    await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
    await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });

    // SECOND authorize: re-navigate the test_app (resets #principal) and drive a
    // REGULAR authorize (no `?sso=`), re-selecting ICRC-3 + the same requested
    // attributes. The last-used SSO identity persists in localStorage across the
    // reload, so the II popup shows the ContinueView "Continue" path.
    await page.goto("https://nice-name.com");
    await page
      .getByRole("textbox", { name: "Identity Provider" })
      .fill(II_URL + "/authorize");
    await page
      .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
      .setChecked(true);
    await page
      .getByRole("checkbox", { name: "Use ICRC-3 attributes:" })
      .setChecked(true);
    await page
      .getByRole("textbox", { name: "Request attributes:" })
      .fill(
        [
          `sso:${SSO_DISCOVERY_DOMAIN}:name`,
          `sso:${SSO_DISCOVERY_DOMAIN}:email`,
        ].join("\n"),
      );
    await expect(page.locator("#principal")).toBeHidden();
    const secondAuthPromise = page.context().waitForEvent("page");
    await page.getByRole("button", { name: "Sign In" }).click();
    const secondAuth = await secondAuthPromise;

    // "Continue" as the last-used SSO identity → re-auth runs the SSO ceremony
    // (`requestWithPopup`). The IdP session established by the first sign-in is
    // reused (`mediation: "optional"`), so the ceremony popup auto-completes
    // without a sign-in form; control returns to the consent screen.
    await secondAuth
      .getByRole("button", { name: "Continue", exact: true })
      .click();

    // LISTED: the consent screen surfaces the SSO-scoped rows (only possible
    // when the re-auth attached the certified bundle). Accept to certify.
    const consent = attributeConsentView(secondAuth);
    await consent.waitForVisible();
    await expect(
      consent.row(`Test SSO ${SSO_OPENID_PORT} email:`),
    ).toBeVisible();
    await expect(
      consent.row(`Test SSO ${SSO_OPENID_PORT} name:`),
    ).toBeVisible();
    await consent.continue();
  });
});
