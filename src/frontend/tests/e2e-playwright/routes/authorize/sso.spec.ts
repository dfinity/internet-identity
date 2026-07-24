import { expect, type APIRequestContext, type Page } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import { DEFAULT_OPENID_PORT } from "../../fixtures/openid";
import {
  SSO_DISCOVERY_DOMAIN,
  SSO_GATING_DISCOVERY_DOMAIN,
  SSO_OPENID_PORT,
  SSO_PER_APP_CLIENT_ID,
  SSO_ENTRA_DISCOVERY_DOMAIN,
  SSO_ENTRA_OPENID_PORT,
  SSO_ENTRA_NAME,
} from "../../fixtures/sso";
import { fromBase64, II_URL, TEST_APP_URL } from "../../utils";

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

// IdP-side per-app SSO gating: the test provider serves `app_clients` /
// `gate_all_apps` in its well-known, so a dapp origin becomes gated (served by a
// per-app client) or denied. Both cases discover through
// SSO_GATING_DISCOVERY_DOMAIN and share one gating config, so nothing mutates it
// between them.
test.describe("Authorize with IdP-side per-app gating", () => {
  const name = "John Doe";
  // Listed in `app_clients` → served by the per-app client; also used as a
  // derivation origin.
  const GATED_ORIGIN = "https://nice-name.com";
  // Absent from `app_clients` → denied by `gate_all_apps`.
  const DENIED_ORIGIN = "https://denied-app.com";

  // Every gated test discovers the same domain, and II caches the SSO
  // well-known per domain (~1h, no force-refresh), so they must share one
  // config — the first fetch wins. `GATED_ORIGIN` maps to the per-app client;
  // every other origin is denied.
  const gatingConfig = {
    appClients: { [GATED_ORIGIN]: SSO_PER_APP_CLIENT_ID },
    gateAllApps: true,
  };

  test.describe("gated origin (assigned user)", () => {
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        testAppURL: GATED_ORIGIN,
        useIcrc3Attributes: true,
        attributes: [
          `sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`,
          `sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`,
        ],
      },
    });

    // Round-trip: the certified bundle must replay through the test_app canister.
    test.afterEach(
      ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
        if (authorizedIcrc3Attributes === undefined) return;
        const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
        expect(canisterEchoedIcrc3Attributes).toEqual(expected);
      },
    );

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      // Non-anonymous principal proves the gated chain completed.
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      // Certified/returned: the gated sign-up attached the SSO bundle.
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(textEntries).toMatchObject({
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`]: email,
      });
    });

    test("first gated login goes through the sign-up prompt, then reaches the app", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
      configureSsoGating,
    }) => {
      // A brand-new gated SSO user flows through the standard "not connected →
      // sign up" prompt (no direct-register shortcut); auto-drive the IdP popup.
      await configureSsoGating(gatingConfig);
      const consent = attributeConsentView(authorizePage.page);
      authorizePage.page.context().on("page", (popup) => {
        void signInWithOpenId(popup, openIdUsers[0].id).catch(() => {
          // A popup can close before it's driven; ignore.
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
      // Fresh SSO user → IdentityNotConnectedDialog; confirm sign-up.
      await authorizePage.page
        .getByRole("dialog")
        .getByRole("button", { name: "Sign up" })
        .click();
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
      // Listed: the consent screen surfaces the SSO-scoped rows.
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

  // 1-click `?sso=` against a gated origin: the ceremony must route to the
  // per-app client at initiate (via the channel origin — GATED_ORIGIN has no
  // derivation origin) so the returned JWT's `aud` matches what the server gate
  // requires. The other gated test drives the manual wizard, which routes by
  // effective origin; this one exercises the `?sso=` path that used to always
  // run against the primary client and so was denied for gated dapps.
  test.describe("gated origin via 1-click ?sso=", () => {
    const email = "john.doe@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
    });

    // Round-trip: the certified bundle must replay through the test_app canister.
    test.afterEach(
      ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
        if (authorizedIcrc3Attributes === undefined) return;
        const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
        expect(canisterEchoedIcrc3Attributes).toEqual(expected);
      },
    );

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      // Non-anonymous principal proves the gated chain completed via 1-click.
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(textEntries).toMatchObject({
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`]: email,
      });
    });

    test("routes to the per-app client and reaches the app", async ({
      page,
      configureSsoGating,
      signInWithOpenId,
      openIdUsers,
    }) => {
      // Gating must be live before the II popup loads: 1-click discovery runs
      // automatically in `onMount`, so we can't lean on the `authorizePage`
      // fixture (it opens the popup during setup, before the body runs). Inline
      // the test_app driving here, after configuring gating.
      await configureSsoGating(gatingConfig);
      await page.goto(GATED_ORIGIN);
      const ssoUrl = `${II_URL}/authorize?sso=${encodeURIComponent(
        SSO_GATING_DISCOVERY_DOMAIN,
      )}`;
      await page
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(ssoUrl);
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
            `sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`,
            `sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`,
          ].join("\n"),
        );
      await expect(page.locator("#principal")).toBeHidden();
      const popupPromise = page.context().waitForEvent("page");
      await page.getByRole("button", { name: "Sign In" }).click();
      const popup = await popupPromise;
      // 1-click auto-approves the SSO allowlist (name, email) and self-closes
      // the popup — no consent screen, no sign-up prompt.
      await signInWithOpenId(popup, openIdUsers[0].id);
      await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });
    });
  });

  // 1-click `?sso=` for a dapp that uses a derivation origin: the derivation
  // origin can't be learned from the channel (it only rides the later delegation
  // request), so it arrives as the `?derivationOrigin=` param. This exercises
  // that param branch in `initiateSso` — discovery keys the derivation origin
  // (`GATED_ORIGIN`), not the channel origin the app is served from.
  test.describe("gated origin via 1-click ?sso= with derivationOrigin", () => {
    const email = "john.doe@example.com";
    // App is served from a second navigable test-app origin; its derivation
    // origin is `GATED_ORIGIN`, which the gate keys on. The gate ignores the
    // channel origin, so hosting here (an otherwise-denied origin) is fine.
    const HOST_ORIGIN = DENIED_ORIGIN;

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
    });

    // Round-trip: the certified bundle must replay through the test_app canister.
    test.afterEach(
      ({ authorizedIcrc3Attributes, canisterEchoedIcrc3Attributes }) => {
        if (authorizedIcrc3Attributes === undefined) return;
        const expected = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
        expect(canisterEchoedIcrc3Attributes).toEqual(expected);
      },
    );

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      // Non-anonymous principal proves the gated chain completed via 1-click.
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const textEntries = decodeIcrc3TextEntries(
        authorizedIcrc3Attributes.data,
      );
      expect(textEntries).toMatchObject({
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`]: name,
        [`sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`]: email,
      });
    });

    test("routes to the per-app client via the derivationOrigin param", async ({
      page,
      configureSsoGating,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await configureSsoGating(gatingConfig);
      await page.goto(HOST_ORIGIN);
      // The derivation origin must list the serving origin in its
      // alternative-origins or the gate rejects it.
      const alternativeOrigins = JSON.stringify({
        alternativeOrigins: [HOST_ORIGIN],
      });
      await page.locator("#hostUrl").fill("https://localhost:5173");
      await page.locator("#newAlternativeOrigins").fill(alternativeOrigins);
      await page.locator("#certified").click();
      await page.locator("#updateNewAlternativeOrigins").click();
      await expect(page.locator("#alternativeOrigins")).toHaveText(
        alternativeOrigins,
        { timeout: 10_000 },
      );
      await page.locator("#derivationOrigin").fill(TEST_APP_URL);

      const ssoUrl = `${II_URL}/authorize?sso=${encodeURIComponent(
        SSO_GATING_DISCOVERY_DOMAIN,
      )}&derivationOrigin=${encodeURIComponent(TEST_APP_URL)}`;
      await page
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(ssoUrl);
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
            `sso:${SSO_GATING_DISCOVERY_DOMAIN}:name`,
            `sso:${SSO_GATING_DISCOVERY_DOMAIN}:email`,
          ].join("\n"),
        );
      await expect(page.locator("#principal")).toBeHidden();
      const popupPromise = page.context().waitForEvent("page");
      await page.getByRole("button", { name: "Sign In" }).click();
      const popup = await popupPromise;
      await signInWithOpenId(popup, openIdUsers[0].id);
      await expect(page.locator("#principal")).toBeVisible({ timeout: 15_000 });
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
      // Origin denied under `gate_all_apps`: discovery resolves the domain but
      // Continue never enables and an inline "not granted" error is shown.
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
      // This flow never authorizes, so close the tab manually or the
      // `authorizePage` fixture teardown (awaits `close`) times out.
      await authorizePage.page.close();
    });
  });

  // Same deny, reached via the 1-click `?sso=` path: discovery runs in the
  // popup's onMount and the gate refuses the origin, so the redemption throws
  // before ever redirecting to the IdP. Unlike the manual wizard (which renders
  // the tailored "not granted" copy), the 1-click path routes the throw through
  // the generic error handler. What matters is that the gate can't be bypassed:
  // the flow errors and the app is never reached.
  test.describe("denied origin via 1-click ?sso=", () => {
    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name } }],
      },
    });

    test("gate_all_apps blocks an unlisted dapp's 1-click", async ({
      page,
      configureSsoGating,
    }) => {
      await configureSsoGating(gatingConfig);
      await page.goto(DENIED_ORIGIN);
      const ssoUrl = `${II_URL}/authorize?sso=${encodeURIComponent(
        SSO_GATING_DISCOVERY_DOMAIN,
      )}`;
      await page
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(ssoUrl);
      await page
        .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
        .setChecked(true);
      await expect(page.locator("#principal")).toBeHidden();
      const popupPromise = page.context().waitForEvent("page");
      await page.getByRole("button", { name: "Sign In" }).click();
      const popup = await popupPromise;
      // Gate deny → the discovery throws `origin-denied`; no IdP redirect, no app.
      await expect(popup.getByText(/origin-denied/i)).toBeVisible({
        timeout: 30_000,
      });
      await expect(
        popup.getByRole("heading", { name: "Sign-in" }),
      ).toBeHidden();
      await expect(page.locator("#principal")).toBeHidden();
    });
  });
});

// The manual wizard entry ("Sign in with SSO" → type the domain) must attach the
// certified SSO bundle like the 1-click path; otherwise the session lists and
// certifies no `sso:<domain>` attributes.
test.describe("Authorize with manual Sign in with SSO", () => {
  const name = "John Doe";
  const email = "john.doe@example.com";

  // Round-trip: the certified bundle must replay through the test_app canister.
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
    // No `sso:` field → the wizard entry, not the `?sso=` 1-click path.
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
    // Certified/returned: the manual session attached the bundle.
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
    // Listed: the consent screen surfaces the SSO-scoped rows.
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

// The last-used SSO re-auth (`authLastUsedFlow.authenticate` behind the
// ContinueView "Continue" button) must redeem through the gate path so the
// certified attribute bundle is attached; otherwise the session lists and
// certifies no `sso:<domain>` attributes.
test.describe("Continue as a last-used SSO identity", () => {
  const name = "John Doe";
  const email = "john.doe@example.com";

  // Round-trip: the certified bundle from the Continue authorize must replay
  // through the test_app canister.
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
    // First authorize (`?sso=` 1-click) seeds the last-used SSO entry; the second
    // drops `?sso=` to run the ContinueView path.
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

    // SECOND authorize: re-navigate test_app and drive a regular authorize (no
    // `?sso=`). The last-used SSO identity persists across the reload, so the II
    // popup shows the ContinueView "Continue" path.
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

    // Continue re-runs the SSO ceremony; the first sign-in's IdP session is
    // reused (`mediation: "optional"`), so the popup auto-completes without a
    // sign-in form.
    await secondAuth
      .getByRole("button", { name: "Continue", exact: true })
      .click();

    // Listed: the consent screen surfaces the SSO-scoped rows. Accept to certify.
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

// Non-`sub` (Entra-style) gating: the org's `sub` is pairwise per OIDC client,
// so a first gated login can't establish the identity from the per-app token.
// The user is prompted for one normal sign-in, then the original per-app token
// is replayed silently and the app opens — no third ceremony, no account-picker.
test.describe("Authorize with gated non-sub (Entra) SSO", () => {
  const name = "Entra User";
  const email = "entra.user@example.com";
  const GATED_ORIGIN = "https://nice-name.com";
  const gatingConfig = {
    domain: SSO_ENTRA_DISCOVERY_DOMAIN,
    appClients: { [GATED_ORIGIN]: SSO_PER_APP_CLIENT_ID },
    gateAllApps: true,
    stableIdentifierClaim: "oid",
  };
  const ssoUrl = `${II_URL}/authorize?sso=${encodeURIComponent(
    SSO_ENTRA_DISCOVERY_DOMAIN,
  )}`;

  test.use({
    openIdConfig: {
      defaultPort: SSO_ENTRA_OPENID_PORT,
      createUsers: [{ claims: { name, email } }],
    },
  });

  // Give the account a fresh `oid` per run. The anchor is keyed by (issuer,
  // primary client, oid) and the e2e canister persists across runs, so a fixed
  // oid would find the anchor already bridged next time — the "first login"
  // dialog only appears for an unbridged one. The account id is a per-run UUID.
  const setFreshOid = (
    request: APIRequestContext,
    accountId: string,
  ): Promise<unknown> =>
    request.post(
      `http://${SSO_ENTRA_DISCOVERY_DOMAIN}/account/${accountId}/claims`,
      { data: { name, email, oid: `entra-oid-${accountId}` } },
    );

  // Start a gated 1-click for the Entra origin and authenticate against the
  // per-app client, returning the IdP popup. Entra discovery is cold on first
  // use (two hops + JWKS via canister outcalls), so wait for the redirect to
  // the IdP before signing in.
  const startGatedLogin = async (
    page: Page,
    signInWithOpenId: (p: Page, id: string) => Promise<void>,
    userId: string,
  ): Promise<Page> => {
    await page.goto(GATED_ORIGIN);
    await page.getByRole("textbox", { name: "Identity Provider" }).fill(ssoUrl);
    await page
      .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
      .setChecked(true);
    await expect(page.locator("#principal")).toBeHidden();
    const popupPromise = page.context().waitForEvent("page");
    await page.getByRole("button", { name: "Sign In" }).click();
    const popup = await popupPromise;
    await expect(popup.getByRole("heading", { name: "Sign-in" })).toBeVisible({
      timeout: 30_000,
    });
    await signInWithOpenId(popup, userId);
    return popup;
  };

  // Complete an IdP popup when the session may already be established: the
  // Sign-in form still shows (prompt=login), but consent can be remembered from
  // an earlier login this session, so the Authorize screen is optional.
  const finishGatedPopupReusingSession = async (
    popup: Page,
    userId: string,
  ): Promise<void> => {
    await expect(popup.getByRole("heading", { name: "Sign-in" })).toBeVisible({
      timeout: 30_000,
    });
    await popup.getByPlaceholder("Enter any login").fill(userId);
    await popup.getByPlaceholder("and password").fill("any-password-works");
    await popup.getByRole("button", { name: "Sign-in" }).click();
    await popup
      .getByRole("button", { name: "Continue" })
      .click({ timeout: 8_000 })
      .catch(() => {
        // Consent remembered from the first login → no Authorize screen; the
        // popup redirects straight back.
      });
  };

  // First gated login for a non-`sub` org: the per-app token can't establish the
  // identity, so II prompts exactly one normal (primary-client) sign-in via the
  // "First sign-in with <org>" dialog, then replays the stashed per-app token —
  // no third ceremony, no "Continue to <app>" account-picker. Leaves the anchor
  // bridged (its `oid` stamped) and the app open.
  const bridgeOnFirstLogin = async (
    page: Page,
    signInWithOpenId: (p: Page, id: string) => Promise<void>,
    userId: string,
  ): Promise<void> => {
    const popup = await startGatedLogin(page, signInWithOpenId, userId);
    await expect(
      popup.getByRole("heading", {
        name: `First sign-in with ${SSO_ENTRA_NAME}`,
      }),
    ).toBeVisible({ timeout: 15_000 });
    const normalPopupPromise = page.context().waitForEvent("page");
    await popup.getByRole("button", { name: "Continue" }).click();
    const normalPopup = await normalPopupPromise;
    await signInWithOpenId(normalPopup, userId);
    await expect(page.locator("#principal")).toBeVisible({ timeout: 25_000 });
  };

  test("first gated 1-click prompts one normal sign-in, then reaches the app", async ({
    page,
    request,
    configureSsoGating,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await configureSsoGating(gatingConfig);
    await setFreshOid(request, openIdUsers[0].id);
    await bridgeOnFirstLogin(page, signInWithOpenId, openIdUsers[0].id);
  });

  test("once bridged, a later gated 1-click resolves in one shot", async ({
    page,
    request,
    configureSsoGating,
    signInWithOpenId,
    openIdUsers,
  }) => {
    await configureSsoGating(gatingConfig);
    await setFreshOid(request, openIdUsers[0].id);
    // First login bridges the anchor (stamps its `oid`).
    await bridgeOnFirstLogin(page, signInWithOpenId, openIdUsers[0].id);

    // Second gated 1-click: the `oid` now resolves the identity, so the per-app
    // token establishes the session directly — no "First sign-in" dialog. Were
    // the dialog to reappear, nobody advances it and #principal below would
    // never show.
    await page.goto(GATED_ORIGIN);
    await page.getByRole("textbox", { name: "Identity Provider" }).fill(ssoUrl);
    await page
      .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
      .setChecked(true);
    await expect(page.locator("#principal")).toBeHidden();
    const popup2Promise = page.context().waitForEvent("page");
    await page.getByRole("button", { name: "Sign In" }).click();
    const popup2 = await popup2Promise;
    await finishGatedPopupReusingSession(popup2, openIdUsers[0].id);
    await expect(page.locator("#principal")).toBeVisible({ timeout: 25_000 });
  });
});
